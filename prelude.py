import logging
import time

class Formatter:

    _DEFAULT_LEVEL_COLOR = '1;37'
    _LEVEL_COLORS = {
        logging.WARNING: '1;35',
        logging.ERROR: '1;31',
        logging.CRITICAL: '1;30;41',
    }

    def __init__(self, color: bool = True) -> None:
        self._color = color

    def _color_tag(self, color: str) -> str:
        return f'\x1b[{color}m' if self._color else ''

    def format(self, record: logging.LogRecord) -> str:
        level_color = self._LEVEL_COLORS.get(
            record.levelno,
            self._DEFAULT_LEVEL_COLOR,
        )
        created = time.localtime(record.created)
        return ''.join([
            self._color_tag('1;37'),
            '[',
            self._color_tag(level_color),
            record.levelname,
            self._color_tag('1;37'),
            '] ',
            time.strftime('%Y-%m-%d %H:%M:%S', created),
            f'.{record.msecs:03.0f}',
            time.strftime(' %Z ', created),
            self._color_tag('0'),
            record.msg % record.args,
        ])

def configure_logging():
    handler = logging.StreamHandler()
    handler.setFormatter(Formatter())
    logging.basicConfig(
        level=logging.WARNING,
        handlers=[handler],
    )
    logging.captureWarnings(True)
