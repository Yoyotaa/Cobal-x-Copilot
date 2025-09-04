from main import MainProgram


def test_process_choice_invalid(capsys):
    app = MainProgram()
    app.process_choice('x')
    captured = capsys.readouterr()
    assert "Invalid choice, please select 1-4." in captured.out
