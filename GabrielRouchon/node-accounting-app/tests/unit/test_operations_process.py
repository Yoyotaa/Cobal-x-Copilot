import builtins
from operations import Operations


def test_process_operation_total(capsys):
    ops = Operations()
    result = ops.process_operation('TOTAL')
    captured = capsys.readouterr()
    assert "Current balance: $1000.00" in captured.out
    assert result == 1000.00


def test_process_operation_credit(monkeypatch, capsys):
    ops = Operations()
    inputs = iter(["100\n"])  # valid credit
    monkeypatch.setattr(builtins, "input", lambda _: next(inputs))
    result = ops.process_operation('CREDIT')
    captured = capsys.readouterr()
    assert "Amount credited. New balance: $1100.00" in captured.out
    assert result == 1100.00


def test_process_operation_debit(monkeypatch, capsys):
    ops = Operations()
    inputs = iter(["100\n"])  # valid debit
    monkeypatch.setattr(builtins, "input", lambda _: next(inputs))
    result = ops.process_operation('DEBIT')
    captured = capsys.readouterr()
    assert "Amount debited. New balance: $900.00" in captured.out
    assert result == 900.00


def test_process_operation_invalid(capsys):
    ops = Operations()
    result = ops.process_operation('UNKNOWN')
    captured = capsys.readouterr()
    assert "Invalid operation type." in captured.out
    assert result is None
