const PREVIEW_BOOT_FLAG = "__sheetlang_preview_bootstrap__";

export type AppRoute = "terminal" | "studio";

const splitPath = (pathname: string) => pathname.split("/").filter(Boolean);

const buildPath = (segments: string[]) => {
	if (segments.length === 0) {
		return "/";
	}
	return `/${segments.join("/")}`;
};

const isPreviewBootstrap = () => {
	const globals = window as unknown as Record<string, unknown>;
	return Boolean(globals[PREVIEW_BOOT_FLAG]);
};

export const getRouteContext = (pathname = window.location.pathname) => {
	const segments = splitPath(pathname);
	const previewBootstrap = isPreviewBootstrap();
	const baseSegments = previewBootstrap && segments.length > 0 ? [segments[0]] : [];
	const appSegments = previewBootstrap ? segments.slice(1) : segments;
	const routeSegment = appSegments[0]?.toLowerCase();
	const route: AppRoute = routeSegment === "studio" ? "studio" : "terminal";

	return {
		route,
		baseSegments,
		appSegments,
	};
};

export const getStudioHref = (pathname = window.location.pathname) => {
	const { baseSegments } = getRouteContext(pathname);
	return buildPath([...baseSegments, "studio"]);
};

export const getTerminalHref = (pathname = window.location.pathname) => {
	const { baseSegments } = getRouteContext(pathname);
	return buildPath(baseSegments);
};

export const getTerminalHrefWithCode = (
	code: string,
	pathname = window.location.pathname,
) => {
	const base = getTerminalHref(pathname);
	if (!code) {
		return base;
	}
	return `${base}?code=${encodeURIComponent(code)}`;
};
