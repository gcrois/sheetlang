/// <reference types="vite/client" />

declare module "*?raw" {
	const content: string;
	export default content;
}

declare module "*.css?raw" {
	const content: string;
	export default content;
}

declare module "*.svg?react" {
	import * as React from "react";
	const Component: React.FC<React.SVGProps<SVGSVGElement>>;
	export default Component;
}
