import React, { createContext, useState } from 'react';

export const ImageModalContext = createContext();

const ImageModalContextProvider = props => {
  const [visible, setVisible] = useState(false);
  return (
    <ImageModalContext.Provider value={{ visible, setVisible }}>
      {props.children}
    </ImageModalContext.Provider>
  );
};
export default ImageModalContextProvider;
