/**
 * Fetch client's public IP once, cache it in localStorage, and automatically
 * attach it to API requests via X-Client-Public-IP.
 */

const STORAGE_KEY = "public_ip";

/**
 * Get cached public IP if available.
 */
function getCachedPublicIp() {
  return localStorage.getItem(STORAGE_KEY);
}

/**
 * Fetch public IP from ipify and cache it.
 */
export async function getPublicIp() {
  const cached = getCachedPublicIp();
  if (cached) {
    return cached;
  }

  try {
    const response = await fetch("https://api.ipify.org?format=json");
    if (!response.ok) {
      throw new Error(`IP lookup failed: ${response.status}`);
    }

    const data = await response.json();
    const ip = data.ip;

    if (ip) {
      localStorage.setItem(STORAGE_KEY, ip);
      return ip;
    }
  } catch (err) {
    console.warn("Unable to determine public IP:", err);
  }

  return null;
}

/**
 * Uses API url to fetch with an added header, 'X-Client-Public-IP' containing client's public IP
 */
export async function fetchWithClientIp(url, options = {}) {
  const ip = await getPublicIp();

  const headers = new Headers(options.headers || {});

  if (ip) {
    headers.set("X-Client-Public-IP", ip);
  }

  return fetch(url, {
    ...options,
    headers,
  });
}

// Expose helpers on window for simple non-module inclusion
if (typeof window !== "undefined") {
  window.getPublicIp = getPublicIp;
  window.fetchWithClientIp = fetchWithClientIp;
}