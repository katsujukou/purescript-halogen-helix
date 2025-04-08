export const _mkSingletonStore = (id_, store) => (() => {
  const id = Symbol.for(id_);
  if (!globalThis[id]) {
    globalThis[id] = store;
  }
  return () => {
    return id;
  };
})();

/**
 * 
 * @param {Symbol} id 
 */
export const _getSingletonStore = (id) => {
  if (!globalThis[id]) {
    throw new Error(`No such a singleton instance: ${id.description}`);
  }
  return globalThis[id];
}
