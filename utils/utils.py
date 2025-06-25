import json
import datetime
import os

def save_history_to_json(self):
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"game_{timestamp}.json"

    data = {
        "timestamp": datetime.datetime.now().isoformat(),
        "history": self.board.history,
        "init_grid": self.board.init_grid,
    }

    root_dir = os.path.dirname(os.path.abspath(__file__))  # file where this function is defined
    project_root = os.path.abspath(os.path.join(root_dir, ".."))  # go up from 'logic', 'game', etc.
    save_dir = os.path.join(project_root, "saved_games")
    os.makedirs(save_dir, exist_ok=True)

    full_path = os.path.join(save_dir, filename)

    with open(full_path, "w") as f:
        json.dump(data, f, indent=2)

    print(f"Game history saved to: {os.path.abspath(full_path)}")

def parse_history_from_json(json_str):
    if isinstance(json_str, dict):
        data = json_str  # Already parsed
    else:
        data = json.loads(json_str)

    return data