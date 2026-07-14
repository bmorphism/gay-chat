#!/usr/bin/env python3
import json
from pathlib import Path

root = Path(__file__).resolve().parents[1]
current = root / "worldview" / "current.md"
topos = root / "worldview" / "consensus-topos.json"

data = json.loads(topos.read_text())
text = current.read_text()

assert data["room"] == "world", data["room"]
assert data["event_count"] == 5, data["event_count"]
assert len(data["observations"]) == 1
assert len(data["active_protentions"]) == 1
assert len(data["open_obstructions"]) == 1
assert len(data["experiments"]) == 1
assert len(data["strong_beliefs"]) == 1
assert "gay://chat worldview — world" in text
assert ("de" + "mo") not in text.lower()

protention = data["active_protentions"][0]
assert protention["kind"] == "protention"
assert protention["color"]["phase"] == "protention"
assert protention["feedback"]["requested"] == [
    "contradiction", "evidence", "experiment-design"
]

obstruction = data["open_obstructions"][0]
assert obstruction["color"]["glue"] == "obstructed"

print("worldview-test ok")
