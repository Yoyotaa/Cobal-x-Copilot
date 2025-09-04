import builtins
from operations import Operations


def test_view_balance(capsys):
    ops = Operations()
    balance = ops.view_balance()
    captured = capsys.readouterr()
    assert "Current balance: $1000.00" in captured.out
    assert balance == 1000.00


def test_credit_account(capsys):
    ops = Operations()
    new_balance = ops.credit_account(200.00)
    captured = capsys.readouterr()
    assert "Amount credited. New balance: $1200.00" in captured.out
    assert new_balance == 1200.00


def test_debit_account_sufficient_funds(capsys):
    ops = Operations()
    ops.credit_account(200.00)  # 1200.00
    new_balance = ops.debit_account(199.99)
    captured = capsys.readouterr()
    assert "Amount debited. New balance: $1000.01" in captured.out
    assert new_balance == 1000.01


def test_debit_account_insufficient_funds(capsys):
    ops = Operations()
    new_balance = ops.debit_account(2000.00)
    captured = capsys.readouterr()
    assert "Insufficient funds for this debit." in captured.out
    assert new_balance == 1000.00


def test_prompt_for_amount_credit(monkeypatch, capsys):
    ops = Operations()
    # Simule l'entrée utilisateur "150\n"
    inputs = iter(["150\n"])  # une seule saisie valide
    monkeypatch.setattr(builtins, "input", lambda _: next(inputs))
    result = ops.prompt_for_amount("credit")
    captured = capsys.readouterr()
    assert "Amount credited. New balance: $1150.00" in captured.out
    assert result == 1150.00


def test_prompt_for_amount_debit_with_retry(monkeypatch, capsys):
    ops = Operations()
    # Enchaîne une valeur invalide, puis une valide
    inputs = iter(["-20\n", "abc\n", "200\n"])  # invalide, invalide, valide
    monkeypatch.setattr(builtins, "input", lambda _: next(inputs))
    result = ops.prompt_for_amount("debit")
    captured = capsys.readouterr()
    # Doit contenir les messages d'erreur et le débit final
    assert "Invalid amount. Please enter a positive number." in captured.out
    assert "Invalid amount. Please enter a valid number." in captured.out
    assert "Amount debited. New balance: $800.00" in captured.out
    assert result == 800.00


