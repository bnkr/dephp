import contextlib, logging

@contextlib.contextmanager
def log_level(level):
    old = logging.getLogger().getEffectiveLevel()
    try:
        logging.getLogger().setLevel(level)
        yield
    finally:
        logging.getLogger().setLevel(old)
