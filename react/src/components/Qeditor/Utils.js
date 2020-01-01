export const isUrl = url => {
  try {
    new URL(url);
    return true;
  } catch (err) {
    return false;
  }
};

export const containsTwitter = url => {
  const regex = /twitter.com.*(status|lists|timelines|moments)/;
  return regex.test(url);
};

export const validTwitterUrl = url => {
  const isValid = isUrl(url);
  if (!isValid) {
    return;
  }
  return containsTwitter(url);
};
