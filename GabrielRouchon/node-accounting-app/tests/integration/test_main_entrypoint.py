import builtins
import importlib
import sys
from types import ModuleType


def test_main_entrypoint_runs_and_exits(monkeypatch, capsys):
    # Prepare input sequence: choose '4' to exit immediately
    inputs = iter(["4\n"])  # selects Exit
    monkeypatch.setattr(builtins, "input", lambda _: next(inputs))

    # Reload the module to ensure __main__ guard doesn't auto-run during import
    if 'main' in sys.modules:
        del sys.modules['main']

    # Simulate running the script as __main__ by executing run() after constructing MainProgram
    import main as main_module

    app = main_module.MainProgram()
    app.run()

    captured = capsys.readouterr()
    assert "Account Management System" in captured.out
    assert "Exiting the program. Goodbye!" in captured.out
