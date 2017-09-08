# a very incomplete attempt at parsing OneNote documents
# https://msdn.microsoft.com/en-us/library/dd951288.aspx
import collections, json, struct, sys

JSON_PRETTY = {
    "ensure_ascii": False,
    "indent": 4,
    "separators": (",", ": "),
    "sort_keys": True,
}

def warn(s):
    sys.stderr.write("warning: {}\n".format(s))
    sys.stderr.flush()

def reverse_hex(h):
    return "".join(reversed([h[i:i+2] for i in range(0, len(h), 2)]))

class Crc32:
    def __init__(self, polynomial, initial, final):
        # https://msdn.microsoft.com/en-us/library/dd923190.aspx
        cache = []
        for i in range(256):
            v = i << 24
            for b in range(8):
                hi = v & (1 << 31)
                v <<= 1
                if hi:
                    v ^= polynomial
            v &= 0xFFFF
            cache.append(v)
        self.cache = cache
        self.initial = initial
        self.final = final

    def calculate(self, crc, b):
        '''To resume, just pass in the same CRC you got last time.'''
        # https://msdn.microsoft.com/en-us/library/dd773252.aspx
        cache = self.cache
        crc ^= self.initial
        for c in b:
            i = (crc >> 24) ^ c
            crc = ((crc << 8) ^ cache[i]) & 0xffffffff
        crc ^= self.final
        return crc

msoCrc32 = Crc32(polynomial=0xAF, initial=0, final=0).calculate

class FileChunkReference64x32(
        collections.namedtuple("FileChunkReference64x32", [
            "stp",
            "cb",
        ])):

    @staticmethod
    def unpack(b):
        return __class__(*struct.unpack_from("<QI", b))

    def deref(self, b):
        return b[self.stp:][:self.cb]

class FileNodeChunkReference(
        collections.namedtuple("FileNodeChunkReference", [
            "stp",
            "cb",
        ])):

    @staticmethod
    def unpack(b, StpFormat, CbFormat):
        fmt = {0: "Q", 1: "I", 2: "H", 3: "I"}[StpFormat]
        stp = struct.unpack_from("<" + fmt, b)[0]
        if StpFormat >= 2:
            stp *= 8
        b = b[struct.calcsize("<" + fmt):]
        fmt = {0: "I", 1: "Q", 2: "B", 3: "H"}[CbFormat]
        cb = struct.unpack("<" + fmt, b)[0]
        if CbFormat >= 2:
            cb *= 8
        return __class__(stp, cb)

    def deref(self, b):
        return b[self.stp:][:self.cb]

def unpack_Header(b):
    s = {}
    s["guidFileType"] = b[:16].hex()
    if s["guidFileType"] != "e4525c7b8cd8a74daeb15378d02996d3":
        warn("unknown guidFileType")
    s["guidFile"] = b[0x10:][:16].hex()
    s["guidFileFormat"] = b[0x30:][:16].hex()
    if s["guidFileFormat"] != "3fdd9a101b91f549a5d01791edc8aed8":
        warn("unknown guidFileFormat")
    s["ffvLastCodeThatWroteToThisFile"] = b[0x40:][:4].hex()
    s["ffvOldestCodeThatHasWrittenToThisFile"] = b[0x44:][:4].hex()
    s["ffvNewestCodeThatHasWrittenToThisFile"] = b[0x48:][:4].hex()
    s["ffvOldestCodeThatMayReadThisFile"] = b[0x4c:][:4].hex()
    s["cTransactionsInLog"], = struct.unpack_from("<I", b[0x60:])
    s["guidAncestor"] = b[0x80:][:16].hex()
    s["crcName"] = b[0x90:][:16].hex()
    s["fcrHashedChunkList"] = FileChunkReference64x32.unpack(b[0x94:])
    s["fcrTransactionLog"] = FileChunkReference64x32.unpack(b[0xa0:])
    s["fcrFileNodeListRoot"] = FileChunkReference64x32.unpack(b[0xac:])
    s["fcrFreeChunkList"] = FileChunkReference64x32.unpack(b[0xb8:])
    s["cbExpectedFileLength"], = struct.unpack_from("<Q", b[0xc4:])
    if len(b) != s["cbExpectedFileLength"]:
        warn("cbExpectedFileLength does not match actual length")
    return s

def unpack_FileNodeListHeader(b):
    s = {}
    (
        s["uintMagic"],
        s["FileNodeListID"],
        s["nFragmentSequence"],
    ) = struct.unpack_from("<QII", b)
    if s["uintMagic"] != 0xa4567ab1f5f7f4c4:
        warn("FileNodeListHeader.uintMagic != 0xa4567ab1f5f7f4c4")
    return s

def unpack_ObjectSpaceManifestRootFND(b, make_ref):
    assert len(b) == 20
    return {
        "gosidRoot": b.hex(),
    }

def unpack_ObjectSpaceManifestListReferenceFND(b, make_ref):
    return {
        "ref": make_ref(b[:-20]),
        "gosid": b[-20:].hex(),
    }

def unpack_ObjectSpaceManifestListStartFND(b, make_ref):
    assert len(b) == 20
    return {
        "gosid": b.hex(),
    }

def unpack_RevisionManifestListReferenceFND(b, make_ref):
    return {
        "ref": make_ref(b),
    }

def unpack_RevisionManifestListStartFND(b, make_ref):
    return {
        "gosid": b[:20].hex(),
        "nInstance": b[20:].hex(),
    }

def unpack_RevisionManifestStart4FND(b, make_ref):
    return {
        "rid": b[:20].hex(),
        "ridDependent": b[20:][:20].hex(),
        "timeCreation": b[40:][:8].hex(),
        "RevisionRole": struct.unpack_from("<i", b[48:]),
        "odcsDefault": struct.unpack_from("<H", b[52:]),
    }

def unpack_GlobalIdTableStartFNDX(b, make_ref):
    return {
        "Reserved": b[0],
    }

def unpack_GlobalIdTableEntryFNDX(b, make_ref):
    return {
        "index": struct.unpack_from("<I", b),
        "guid": b[4:][16:].hex(),
    }

def unpack_GlobalIdTableEndFNDX(b, make_ref):
    assert len(b) == 0
    return {}

def unpack_ObjectDeclarationWithRefCountFNDX(b, make_ref):
    return {
        "ObjectRef": make_ref(b[:-11]),
        "body": b[-11:-1].hex(),
        "cRef": b[-1],
    }

def unpack_CompactID(b):
    u, = struct.unpack_from("<I", b)
    return {
        "n": u & 0xff,
        "guidIndex": (u >> 8) & 0xffffff,
    }

def unpack_RootObjectReference2FNDX(b, make_ref):
    return {
        "oidRoot": unpack_CompactID(b),
        "RootRole": struct.unpack_from("<I", b[4:]),
    }

def classify_FileNode(fileNode):
    s = fileNode
    if s["FileNodeID"] == 0x0ff:
        return "ChunkTerminatorFND"
    fileNodeType = {
        (0x004, 0): "ObjectSpaceManifestRootFND",
        (0x008, 2): "ObjectSpaceManifestListReferenceFND",
        (0x00C, 0): "ObjectSpaceManifestListStartFND",
        (0x010, 2): "RevisionManifestListReferenceFND",
        (0x014, 0): "RevisionManifestListStartFND",
        (0x01b, 0): "RevisionManifestStart4FND",
        (0x021, 0): "GlobalIdTableStartFNDX",
        (0x024, 0): "GlobalIdTableEntryFNDX",
        (0x028, 0): "GlobalIdTableEndFNDX",
        (0x02d, 1): "ObjectDeclarationWithRefCountFNDX",
        (0x059, 0): "RootObjectReference2FNDX",
        (0x084, 1): "ObjectInfoDependencyOverridesFND",
    }.get((s["FileNodeID"], s["BaseType"]))
    if fileNodeType is None:
        warn("unknown FileNode (FileNodeID={}, BaseType={})".format(
            hex(s["FileNodeID"]), s["BaseType"]))
        return "_unimplemented"
    return fileNodeType

def unpack_FileNode(b):
    h, = struct.unpack_from("<I", b)
    s = {}
    s["FileNodeID"] = h & ((1 << 10) - 1)
    s["Size"] = (h >> 10) & ((1 << 13) - 1)
    s["StpFormat"] = (h >> 23) & 0b11
    s["CbFormat"] = (h >> 25) & 0b11
    s["BaseType"] = (h >> 27) & 0b1111
    Reserved = (h >> 31) & 0b1
    if s["Size"] < 4:
        warn("FileNode.Size < 4")
    fnd = b[4:s["Size"]]
    b = b[max(4, s["Size"]):]
    fileNodeType = classify_FileNode(s)
    s["_fileNodeType"] = fileNodeType
    try:
        unpack = globals()["unpack_" + fileNodeType]
    except KeyError:
        s["fnd"] = "_unimplemented"
    else:
        def make_ref(b):
            return FileNodeChunkReference.unpack(
                b, s["StpFormat"], s["CbFormat"])
        s["fnd"] = unpack(fnd, make_ref)
    if Reserved != 1:
        warn("FileNode.Reserved != 1")
    return s, b

def unpack_FileNodeListFragment(b, num_file_nodes):
    s = {}
    s["header"] = unpack_FileNodeListHeader(b)
    bb = b[0x10:-0x10]
    s["rgFileNodes"] = []
    while len(bb) >= 4 and len(s["rgFileNodes"]) < num_file_nodes:
        node, bb = unpack_FileNode(bb)
        s["rgFileNodes"].append(node)
        if node["_fileNodeType"] == "ChunkTerminatorFND":
            break
    s["nextFragment"] = FileChunkReference64x32.unpack(b[-20:])
    s["footer"], = struct.unpack_from("<Q", b[-8:])
    if s["footer"] != 0x8bc215c38233ba4b:
        warn("FileNodeListFragment.footer != 0x8bc215c38233ba4b")
    return s

def parse_FileNodeList(b, fcr, sizeTable):
    header = unpack_FileNodeListHeader(fcr.deref(b))
    fileNodeList = {
        "FileNodeListID": header["FileNodeListID"],
        "rgFileNodes": [],
    }
    num_file_nodes = sizeTable[fileNodeList["FileNodeListID"]]
    while num_file_nodes > 0:
        fileNodeListFragment = unpack_FileNodeListFragment(
            fcr.deref(b), num_file_nodes)
        num_file_nodes -= len(fileNodeListFragment["rgFileNodes"])
        fileNodeList["rgFileNodes"].extend(fileNodeListFragment["rgFileNodes"])
        fcr = fileNodeListFragment["nextFragment"]
    return fileNodeList

def unpack_OIDs(b):
    u, = struct.unpack_from("<I", b)
    b = b[4:]
    s = {"header": {}}
    s["header"]["Count"] = u & 0xffffff
    s["header"]["ExtendedStreamsPresent"] = (u >> 30) & 1
    s["header"]["OsidStreamNotPresent"] = (u >> 31) & 1
    s["body"] = [unpack_CompactID(b[i * 4:])
                 for i in range(s["header"]["Count"])]
    b = b[s["header"]["Count"] * 4:]
    return s, b

def unpack_ObjectSpaceObjectPropSet(b):
    s = {}
    s["OIDs"], b = unpack_OIDs(b)
    return s

def unpack_TransactionEntry(b):
    s = {}
    s["srcID"], s["TransactionEntrySwitch"] = struct.unpack_from("<II", b)
    return s

def pack_TransactionEntry(transactionEntry):
    return struct.pack(
        "<II",
        transactionEntry["srcID"],
        transactionEntry["TransactionEntrySwitch"],
    )

def unpack_TransactionLogFragment(b, cTransactionsInLog):
    s = {}
    s["sizeTable"] = []
    bb = b[:-12]
    while cTransactionsInLog and bb:
        s["sizeTable"].append(unpack_TransactionEntry(bb))
        bb = bb[8:]
        if s["sizeTable"][-1]["srcID"] == 0x00000001: # sentinel
            cTransactionsInLog -= 1
    s["nextFragment"] = FileChunkReference64x32.unpack(b[-12:])
    return s, cTransactionsInLog

def iter_TransactionEntries(b, header):
    nextFragment = header["fcrTransactionLog"]
    cTransactionsInLog = header["cTransactionsInLog"]
    transaction = []
    crc_buffer = bytearray()
    crc = 0
    # FIXME: is it always this algorithm?
    crc32 = msoCrc32
    while cTransactionsInLog:
        transactionEntries, cTransactionsInLog = unpack_TransactionLogFragment(
            nextFragment.deref(b), cTransactionsInLog)
        nextFragment = transactionEntries["nextFragment"]
        for transactionEntry in transactionEntries["sizeTable"]:
            if transactionEntry["srcID"] == 0x00000001: # sentinel
                crc = crc32(crc, crc_buffer)
                expected_crc = transactionEntry["TransactionEntrySwitch"]
                if crc != expected_crc:
                    warn("transaction CRC mismatch: {:08x} != {:08x}"
                         .format(crc, expected_crc))
                yield transaction
                crc_buffer = bytearray()
                transaction = []
            else:
                transaction.append(transactionEntry)
            crc_buffer.extend(pack_TransactionEntry(transactionEntry))

def get_sizeTable(b, header):
    # replay the transactions to compute the sizeTable
    sizeTable = {}
    for transaction in iter_TransactionEntries(b, header):
        for transactionEntry in transaction:
            sizeTable[transactionEntry["srcID"]] = (
                transactionEntry["TransactionEntrySwitch"]
            )
    return sizeTable

def find_fileNodes(fileNodes, fileNodeType):
    return [
        fileNode["fnd"]
        for fileNode in fileNodes["rgFileNodes"]
        if fileNode["_fileNodeType"] == fileNodeType
    ]

with open(sys.argv[1], "rb") as f:
    b = f.read()

header = unpack_Header(b)
print("header", json.dumps(header, **JSON_PRETTY), flush=True)

sizeTable = get_sizeTable(b, header)
print("sizeTable", json.dumps(sizeTable, **JSON_PRETTY), flush=True)

rootNodeList = parse_FileNodeList(b, header["fcrFileNodeListRoot"], sizeTable)
print("rootNodeList", json.dumps(rootNodeList, **JSON_PRETTY), flush=True)

[objectSpaceManifestRootFND] = find_fileNodes(
    rootNodeList, "ObjectSpaceManifestRootFND")
gosidRoot = objectSpaceManifestRootFND["gosidRoot"]
[rootObjectSpaceManifestListReferenceFND] = [
    fnd
    for fnd in find_fileNodes(
            rootNodeList,
            "ObjectSpaceManifestListReferenceFND",
    )
    if fnd["gosid"] == gosidRoot
]

rootObjectSpaceManifestList = parse_FileNodeList(
    b, rootObjectSpaceManifestListReferenceFND["ref"], sizeTable)
print("rootObjectSpaceManifestList",
      json.dumps(rootObjectSpaceManifestList, **JSON_PRETTY),
      flush=True)

# pick the first one (?)
revisionManifestList0 = parse_FileNodeList(
    b,
    find_fileNodes(
        rootObjectSpaceManifestList,
        "RevisionManifestListReferenceFND",
    )[0]["ref"],
    sizeTable,
)
print("revisionManifestList0",
      json.dumps(revisionManifestList0, **JSON_PRETTY),
      flush=True)

objectDeclarationWithRefCount = unpack_ObjectSpaceObjectPropSet(
    find_fileNodes(
        revisionManifestList0,
        "ObjectDeclarationWithRefCountFNDX",
    )[0]["ObjectRef"].deref(b),
)
print("objectDeclarationWithRefCount",
      json.dumps(objectDeclarationWithRefCount, **JSON_PRETTY),
      flush=True)
