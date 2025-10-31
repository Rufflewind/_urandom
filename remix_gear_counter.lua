function(event)
    local wantExpansionID = LE_EXPANSION_LEGION
    local excludedBonusIDs = {
        [12694] = true,
        [12695] = true,
        [12696] = true,
        [13357] = true
    }
    
    local aura_env = aura_env
    
    if aura_env.TooltipPostCall == nil then
        aura_env.TooltipPostCall = function() end
        TooltipDataProcessor.AddTooltipPostCall(Enum.TooltipDataType.Item, function(self) aura_env.TooltipPostCall(self) end)
    end
    
    if event == "BANKFRAME_OPENED" then
        aura_env.BankOpen = true
    elseif event == "BANKFRAME_CLOSED" then
        aura_env.BankOpen = false
    end
    if not aura_env.BankOpen then
        aura_env.TooltipPostCall = function() end
        return false
    end
    
    local ITEM_EQUIP_LOC_MAP = {
        ["INVTYPE_ROBE"] = "INVTYPE_CHEST"
    }
    
    local function createCounter()
        return {equipped = 0, bags = 0, bank = 0}
    end
    
    local function totalCount(counter)
        return counter.equipped + counter.bags + counter.bank
    end
    
    local function computeSignature(itemLink)
        if not itemLink then
            return nil
        end
        local _, _, itemQuality, itemLevel, itemMinLevel, _, _, itemStackCount, itemEquipLoc, _, sellPrice, classID, subclassID, bindType, expansionID = C_Item.GetItemInfo(itemLink)
        if not (itemStackCount == 1
            and (classID == Enum.ItemClass.Weapon or classID == Enum.ItemClass.Armor)
            and bindType == Enum.ItemBind.OnAcquire
        and expansionID == wantExpansionID) then
            return nil
        end
        local _, linkOptions = LinkUtil.ExtractLink(itemLink)
        local splitOptions = {LinkUtil.SplitLinkOptions(linkOptions)}
        local numBonusIDs = tonumber(splitOptions[13])
        if numBonusIDs == nil then
            return nil
        end
        local bonusIDs = {}
        for i = 1, numBonusIDs do
            local bonusID = splitOptions[13 + i]
            if not excludedBonusIDs[tonumber(bonusID)] then
                tinsert(bonusIDs, bonusID)
            end
        end
        sort(bonusIDs)
        return format(
            "%d,%d,%s,%d,%d,%s",
            itemQuality,
            itemLevel,
            ITEM_EQUIP_LOC_MAP[itemEquipLoc] or itemEquipLoc,
            classID,
            subclassID,
            table.concat(bonusIDs, ":")
        )
    end
    
    local function processItem(bagType, counterBySignature, itemLink)
        local signature = computeSignature(itemLink)
        if not signature then
            return
        end
        local counter = counterBySignature[signature]
        if counter == nil then
            counter = createCounter()
            counterBySignature[signature] = counter
        end
        counter[bagType] = counter[bagType] + 1
    end
    
    local function processBag(bagType, counterBySignature, bag)
        for slot = 1, C_Container.GetContainerNumSlots(bag) do
            local itemLink = C_Container.GetContainerItemLink(bag, slot)
            processItem(bagType, counterBySignature, itemLink)
        end
    end
    
    local function scanItems()
        local counterBySignature = {}
        for invSlotId = 1, 30 do
            local itemLink = GetInventoryItemLink("player", invSlotId)
            processItem("equipped", counterBySignature, itemLink)
        end
        for bag = BACKPACK_CONTAINER, ITEM_INVENTORY_BANK_BAG_OFFSET do
            processBag("bags", counterBySignature, bag)
        end
        for bag = ITEM_INVENTORY_BANK_BAG_OFFSET + 1, ITEM_INVENTORY_BANK_BAG_OFFSET + Constants.InventoryConstants.NumCharacterBankSlots do
            processBag("bank", counterBySignature, bag)
        end
        return counterBySignature
    end
    
    local lastSignature = nil
    local counterBySignature = {}
    function aura_env.TooltipPostCall(self)
        if self.GetItem == nil then
            return
        end
        local _, itemLink = self:GetItem()
        local signature = computeSignature(itemLink)
        if not signature then
            return
        end
        if lastSignature ~= signature then
            counterBySignature = scanItems()
            lastSignature = signature
        end
        local counter = counterBySignature[signature]
        if counter == nil then
            return                    -- Sometimes the item info is not yet cached.
        end
        local total = totalCount(counter)
        local color = "ff00FFFF"
        if total > 1 then
            color = "ffFF0000"
        end
        self:AddDoubleLine("RemixCounter", format("\124c%s%d (Bags: %d, Bank: %d, Equipped: %d)\124r", color, total, counter.bags, counter.bank, counter.equipped))
        if aura_env.config.debugMode then
            self:AddLine(signature)
        end
    end
    
    return true
end
