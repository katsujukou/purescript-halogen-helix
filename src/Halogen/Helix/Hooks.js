export const unsafeSelect = (selector) => (state) => {
    try {
        return selector(state);
    }
    catch (e) {
        console.log(e);
        return state;
    }
}