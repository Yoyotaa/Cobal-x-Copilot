from data import DataProgram


def test_initial_balance():
    data = DataProgram()
    assert data.get_balance() == 1000.00


def test_write_and_read_updates_balance():
    data = DataProgram()
    data.write(1234.56)
    assert data.read() == 1234.56


def test_set_and_get_balance():
    data = DataProgram()
    data.set_balance(42.0)
    assert data.get_balance() == 42.0


