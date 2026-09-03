export async function onRequest(context) {
  const url = new URL(context.request.url);
  const plausiblePath = url.pathname.replace(/^\/pio/, "") + url.search;

  const response = await fetch("https://plausible.io" + plausiblePath, {
    method: context.request.method,
    headers: context.request.headers,
    body: context.request.method === "POST" ? context.request.body : undefined,
  });

  // Return with CORS-safe headers
  const newHeaders = new Headers(response.headers);
  newHeaders.delete("set-cookie");

  return new Response(response.body, {
    status: response.status,
    headers: newHeaders,
  });
}
