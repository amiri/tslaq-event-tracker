export const isUrl = url => {
  try {
    new URL(url);
    return true;
  } catch (err) {
    return false;
  }
};
