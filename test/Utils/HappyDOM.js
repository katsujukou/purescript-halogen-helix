import { Window } from 'happy-dom';


export const window = () => {
  let w = new Window();
  global.window = w;
  return w
}

/**
 * 
 * @param {import('happy-dom').Window} window 
 */
export const _waitUntilComplete = async (window) => window.happyDOM.waitUntilComplete();