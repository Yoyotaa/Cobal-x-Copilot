import pytest
from unittest.mock import patch
import main
import operations
BALANCE_FILE = "balance.txt"


def test_invalid_choice(capsys):
    with patch("builtins.input", side_effect=["5", "aaaa", "4"]):
        main.main()
    captured = capsys.readouterr()
    #print("aaaaaaaaa")
    #print(captured.out)
    #print("bbbbbbbbbb")
    assert "Invalid choice, please select 1-4." in captured.out
    assert "Invalid choice, please select 1-4." in captured.out
    assert "Exiting the program. Goodbye!" in captured.out

def test_exit_program(capsys):
    with patch("builtins.input", side_effect=["4"]):
        main.main()
    captured = capsys.readouterr()
    #print(captured.out)
    assert "Exiting the program. Goodbye!" in captured.out

def test_view_balance(capsys):
    try: 
        with open(BALANCE_FILE, "r") as f:
            balance = float(f.read())
    except FileNotFoundError:
        balance = 1000.0
    with patch("builtins.input", side_effect=["1", "4"]):
        main.main()
    captured = capsys.readouterr()
    balancestr = str(balance)
    testone = "Current balance: " + balancestr
    lines = captured.out.splitlines()
    #print(captured.out)
    assert testone in captured.out
    assert "Exiting the program. Goodbye!" in captured.out

def test_add_positive_credit(capsys):
    try: 
        with open(BALANCE_FILE, "r") as f:
            balance = float(f.read())
    except FileNotFoundError:
        balance = 1000.0
    with patch("builtins.input", side_effect=["2", "300", "4"]):
        main.main()
    captured = capsys.readouterr()
    balancestr = str(balance + 300)
    testone = "Amount credited. New balance: " + balancestr
    lines = captured.out.splitlines()
    #print(captured.out)

    assert testone in captured.out
    assert "Exiting the program. Goodbye!" in captured.out

def test_negative_credit(capsys):
    try: 
        with open(BALANCE_FILE, "r") as f:
            balance = float(f.read())
    except FileNotFoundError:
        balance = 1000.0
    with patch("builtins.input", side_effect=["2", "-300", "4"]):
        main.main()
    captured = capsys.readouterr()
    balancestr = str(balance + 300)
    testone = "Invalid input. Please enter a positive numeric value for the credit amount."
    lines = captured.out.splitlines()
    assert testone in captured.out
    assert "Exiting the program. Goodbye!" in captured.out

def test_invalid_credit(capsys):
    try: 
        with open(BALANCE_FILE, "r") as f:
            balance = float(f.read())
    except FileNotFoundError:
        balance = 1000.0
    with patch("builtins.input", side_effect=["2", "aaaa", "4"]):
        main.main()
    captured = capsys.readouterr()
    balancestr = str(balance + 300)
    testone = "Invalid input. Please enter a positive numeric value for the credit amount."
    lines = captured.out.splitlines()
    assert testone in captured.out
    assert "Exiting the program. Goodbye!" in captured.out

def test_debit_valid_funds(capsys):
    try: 
        with open(BALANCE_FILE, "r") as f:
            balance = float(f.read())
    except FileNotFoundError:
        balance = 1000.0
    with patch("builtins.input", side_effect=["3", "200", "4"]):
        main.main()
    captured = capsys.readouterr()
    balancestr = str(balance - 200)
    testone = "Amount debited. New balance: " + balancestr
    lines = captured.out.splitlines()
    assert testone in captured.out
    assert "Exiting the program. Goodbye!" in captured.out    

def test_debit_valid_funds(capsys):
    try: 
        with open(BALANCE_FILE, "r") as f:
            balance = float(f.read())
    except FileNotFoundError:
        balance = 1000.0
    with patch("builtins.input", side_effect=["3", "200", "4"]):
        main.main()
    captured = capsys.readouterr()
    balancestr = str(balance - 200)
    testone = "Amount debited. New balance: " + balancestr
    lines = captured.out.splitlines()
    assert testone in captured.out
    assert "Exiting the program. Goodbye!" in captured.out

def test_debit_toomuch_funds(capsys):
    try: 
        with open(BALANCE_FILE, "r") as f:
            balance = float(f.read())
    except FileNotFoundError:
        balance = 1000.0
    balance_bis = str(balance*2)
    with patch("builtins.input", side_effect=["3", balance_bis, "4"]):
        main.main()
    captured = capsys.readouterr()
    balancestr = str(balance - 200)
    testone = "Insufficient funds for this debit."
    lines = captured.out.splitlines()
    assert testone in captured.out
    assert "Exiting the program. Goodbye!" in captured.out
      
def test_debit_negative_funds(capsys):
    try: 
        with open(BALANCE_FILE, "r") as f:
            balance = float(f.read())
    except FileNotFoundError:
        balance = 1000.0
    with patch("builtins.input", side_effect=["3", "-210", "4"]):
        main.main()
    captured = capsys.readouterr()
    balancestr = str(balance - 200)
    testone = "Invalid input. Please enter a positive numeric value for the debit amount."
    assert testone in captured.out
    assert "Exiting the program. Goodbye!" in captured.out

def test_debit_invalid_number(capsys):
    try: 
        with open(BALANCE_FILE, "r") as f:
            balance = float(f.read())
    except FileNotFoundError:
        balance = 1000.0
    with patch("builtins.input", side_effect=["3","aaaaa" , "4"]):
        main.main()
    captured = capsys.readouterr()
    balancestr = str(balance - 200)
    testone = "Invalid input. Please enter a positive numeric value for the debit amount."
    assert testone in captured.out
    assert "Exiting the program. Goodbye!" in captured.out