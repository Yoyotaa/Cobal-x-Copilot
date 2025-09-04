import builtins
import runpy


def test_running_main_via_runpy_exits(monkeypatch, capsys):
    # Simulate choosing '4' to exit immediately
    inputs = iter(["4\n"])  # Exit
    monkeypatch.setattr(builtins, "input", lambda _: next(inputs))

    # Execute the module as if run with python main.py
    runpy.run_module('main', run_name='__main__')

    out = capsys.readouterr().out
    assert "Account Management System" in out
    assert "Exiting the program. Goodbye!" in out
