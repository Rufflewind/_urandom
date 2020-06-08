import decimal, re

import yaml

class YamlDumper(yaml.SafeDumper):
    pass

def _decimal_representer(dumper: yaml.Dumper, data: decimal.Decimal) -> yaml.Node:
    return dumper.represent_scalar('tag:yaml.org,2002:float', str(data))

def _none_representer(dumper: yaml.Dumper, data: None) -> yaml.Node:
    return dumper.represent_scalar('tag:yaml.org,2002:null', '')

YamlDumper.add_representer(decimal.Decimal, _decimal_representer)
YamlDumper.add_representer(type(None), _none_representer)

class YamlLoader(yaml.SafeLoader):
    pass

def _decimal_constructor(loader: yaml.Loader, node: yaml.Node) -> decimal.Decimal:
    return decimal.Decimal(loader.construct_scalar(node))

YamlLoader.add_constructor('!decimal', lambda loader, node: decimal.Decimal)
YamlLoader.add_implicit_resolver(
    '!decimal',
    re.compile(r'(?a)[-+]?(\.\d+|\d+(\.\d*)?)([Ee][-+]?\d+)?'),
    '-+0123456789.',
)
