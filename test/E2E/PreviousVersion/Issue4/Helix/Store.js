export const unsafeGetOrCache = (() => {
  let cache = {};
  return (id, deferred) => {
    if (!(id in cache)) {
      cache[id] = deferred();
    }
    return cache[id];
  }
})();