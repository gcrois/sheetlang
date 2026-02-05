const PAGES_SUFFIX = ".pages.dev";
const PROBE_TIMEOUT_MS = 2500;
const ENTRY_SCRIPT_REGEX = /<script[^>]+type=["']module["'][^>]+src=["']([^"']+)["'][^>]*>/i;

const loadMain = async () => {
    await import("./main.tsx");
};

const isPreviewHost = (hostname: string) => {
    if (!hostname.endsWith(PAGES_SUFFIX)) {
        return false;
    }

    const subdomain = hostname.slice(0, -PAGES_SUFFIX.length);
    return subdomain.includes(".");
};

const normalizeSlug = (value: string) => {
    return value
        .toLowerCase()
        .replace(/[^a-z0-9-]+/g, "-")
        .replace(/-+/g, "-")
        .replace(/^-+|-+$/g, "")
        .trim();
};

const getRouteSegment = (pathname: string) => {
    const parts = pathname.split("/").filter(Boolean);
    return parts.length > 0 ? parts[0] : null;
};

const getRemainderPath = (pathname: string) => {
    const parts = pathname.split("/").filter(Boolean);
    if (parts.length <= 1) {
        return "/";
    }

    return `/${parts.slice(1).join("/")}`;
};

const resolvePagesHost = () => {
    const envHost = import.meta.env.VITE_PAGES_HOST as string | undefined;
    if (!envHost) {
        return null;
    }

    const trimmed = envHost.trim();
    if (trimmed.length === 0) {
        return null;
    }

    if (trimmed.includes("://")) {
        try {
            return new URL(trimmed).host;
        } catch {
            return null;
        }
    }

    return trimmed.replace(/\/.*$/, "");
};

const fetchWithTimeout = async (url: string, init: RequestInit = {}) => {
    const controller = new AbortController();
    const timeoutId = window.setTimeout(() => controller.abort(), PROBE_TIMEOUT_MS);
    try {
        return await fetch(url, {
            cache: "no-store",
            mode: "cors",
            ...init,
            signal: controller.signal,
        });
    } finally {
        window.clearTimeout(timeoutId);
    }
};

const resolveEntryScript = async (origin: string) => {
    const response = await fetchWithTimeout(`${origin}/index.html`, { method: "GET" });
    if (!response.ok) {
        return null;
    }

    const html = await response.text();
    const match = html.match(ENTRY_SCRIPT_REGEX);
    if (!match) {
        return null;
    }

    return new URL(match[1], origin).toString();
};

const probePreviewOrigin = async (origin: string) => {
    try {
        const entryUrl = await resolveEntryScript(origin);
        if (!entryUrl) {
            return false;
        }

        const response = await fetchWithTimeout(entryUrl, { method: "GET" });
        return response.ok;
    } catch {
        return false;
    }
};

const bootstrap = async () => {
    const route = getRouteSegment(window.location.pathname);
    if (!route) {
        await loadMain();
        return;
    }

    if (isPreviewHost(window.location.hostname)) {
        await loadMain();
        return;
    }

    const slug = normalizeSlug(route);
    if (!slug) {
        await loadMain();
        return;
    }

    const pagesHost = resolvePagesHost();
    if (!pagesHost) {
        await loadMain();
        return;
    }

    const origin = `https://${slug}.${pagesHost}`;
    const exists = await probePreviewOrigin(origin);
    if (!exists) {
        await loadMain();
        return;
    }

    const url = new URL(origin);
    url.pathname = getRemainderPath(window.location.pathname);
    url.search = window.location.search;
    url.hash = window.location.hash;
    window.location.replace(url.toString());
};

void bootstrap();
