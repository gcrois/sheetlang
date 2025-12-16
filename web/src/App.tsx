import "./App.css";
import { Sheet } from "../../pkg"
import { useEffect } from "react";

function App() {
    useEffect(() => {
        const sheet = new Sheet();
        sheet.set_formula("A1", "10");
        sheet.set_formula("A2", "20");
        sheet.set_formula("A3", "A1 + A2");
        sheet.tick();
        sheet.tick();
        const value = sheet.get_all_values();
        console.log("Values: ", value);
    })

	return (
		<div>
            I'm just a little baby App
        </div>
	);
}

export default App;
