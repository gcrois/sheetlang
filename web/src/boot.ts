const PAGES_SUFFIX = ".pages.dev";
const PROBE_TIMEOUT_MS = 2500;

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

const isPreviewBootstrap = () => {
    return Boolean(window.__sheetlang_preview_bootstrap__);
};

const markPreviewBootstrap = () => {
    window.__sheetlang_preview_bootstrap__ = true;
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

type PreviewAssets = {
    scriptUrl: string;
    stylesheetUrls: string[];
    modulepreloadUrls: string[];
};

const resolveEntryAssets = async (origin: string) => {
    const response = await fetchWithTimeout(`${origin}/index.html`, { method: "GET" });
    if (!response.ok) {
        return null;
    }

    const html = await response.text();
    const doc = new DOMParser().parseFromString(html, "text/html");
    const script = doc.querySelector('script[type="module"][src]');
    if (!script) {
        return null;
    }

    const scriptSrc = script.getAttribute("src");
    if (!scriptSrc) {
        return null;
    }

    const stylesheetUrls = Array.from(doc.querySelectorAll('link[rel="stylesheet"][href]'))
        .map((link) => link.getAttribute("href"))
        .filter((href): href is string => Boolean(href))
        .map((href) => new URL(href, origin).toString());

    const modulepreloadUrls = Array.from(doc.querySelectorAll('link[rel="modulepreload"][href]'))
        .map((link) => link.getAttribute("href"))
        .filter((href): href is string => Boolean(href))
        .map((href) => new URL(href, origin).toString());

    return {
        scriptUrl: new URL(scriptSrc, origin).toString(),
        stylesheetUrls,
        modulepreloadUrls,
    } satisfies PreviewAssets;
};

const ensureLinkTag = (rel: string, href: string) => {
    const selector = `link[rel="${rel}"][href="${href}"]`;
    if (document.querySelector(selector)) {
        return null;
    }

    const link = document.createElement("link");
    link.rel = rel;
    link.href = href;
    link.crossOrigin = "anonymous";
    document.head.appendChild(link);
    return link;
};

const loadPreviewAssets = (assets: PreviewAssets) => {
    const inserted: HTMLElement[] = [];

    for (const href of assets.modulepreloadUrls) {
        const link = ensureLinkTag("modulepreload", href);
        if (link) {
            inserted.push(link);
        }
    }

    for (const href of assets.stylesheetUrls) {
        const link = ensureLinkTag("stylesheet", href);
        if (link) {
            inserted.push(link);
        }
    }

    const script = document.createElement("script");
    script.type = "module";
    script.src = assets.scriptUrl;
    script.crossOrigin = "anonymous";
    script.addEventListener("error", () => {
        for (const node of inserted) {
            node.remove();
        }
        script.remove();
        void loadMain();
    });
    document.head.appendChild(script);
};

const bootstrap = async () => {
    if (isPreviewBootstrap()) {
        await loadMain();
        return;
    }

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
    let assets: PreviewAssets | null = null;
    try {
        assets = await resolveEntryAssets(origin);
    } catch {
        assets = null;
    }

    if (!assets) {
        await loadMain();
        return;
    }

    markPreviewBootstrap();
    loadPreviewAssets(assets);
};

void bootstrap();
