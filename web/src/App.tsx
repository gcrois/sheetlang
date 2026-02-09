import { useEffect, useState } from "react";
import TerminalApp from "./TerminalApp";
import StudioApp from "./StudioApp";
import { AppRoute, getRouteContext } from "./routing";

const resolveRoute = (): AppRoute => getRouteContext().route;

function App() {
	const [route, setRoute] = useState(resolveRoute);

	useEffect(() => {
		const handleRoute = () => {
			setRoute(resolveRoute());
		};

		window.addEventListener("popstate", handleRoute);
		window.addEventListener("hashchange", handleRoute);

		return () => {
			window.removeEventListener("popstate", handleRoute);
			window.removeEventListener("hashchange", handleRoute);
		};
	}, []);

	if (route === "studio") {
		return <StudioApp />;
	}

	return <TerminalApp />;
}

export default App;
