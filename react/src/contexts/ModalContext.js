import React, { createContext, useState } from 'react';

export const ModalContext = createContext();

const ModalContextProvider = props => {
  const [visible, setVisible] = useState(false);
  return (
    <ModalContext.Provider value={{ visible, setVisible }}>
      {props.children}
    </ModalContext.Provider>
  );
};
export default ModalContextProvider;
