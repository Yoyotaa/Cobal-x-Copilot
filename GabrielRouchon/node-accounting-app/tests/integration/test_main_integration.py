import builtins
from main import MainProgram


def test_main_menu_flow_view_then_exit(monkeypatch, capsys):
    main = MainProgram()

    # Simule: afficher le solde puis quitter
    inputs = iter(["1\n", "4\n"])  # view balance, exit
    monkeypatch.setattr(builtins, "input", lambda _: next(inputs))

    main.run()

    out = capsys.readouterr().out
    assert "Account Management System" in out
    assert "Current balance: $1000.00" in out
    assert "Exiting the program. Goodbye!" in out


def test_main_menu_flow_credit_and_debit_then_exit(monkeypatch, capsys):
    main = MainProgram()

    # Simule: crédit 300, débit 100, afficher, quitter
    inputs = iter([
        "2\n",  # choose credit
        "300\n",  # amount
        "3\n",  # choose debit
        "100\n",  # amount
        "1\n",  # view balance
        "4\n",  # exit
    ])
    monkeypatch.setattr(builtins, "input", lambda _: next(inputs))

    main.run()

    out = capsys.readouterr().out
    assert "Amount credited. New balance: $1300.00" in out
    assert "Amount debited. New balance: $1200.00" in out
    assert "Current balance: $1200.00" in out
    assert "Exiting the program. Goodbye!" in out


