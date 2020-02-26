import React, { createContext, useState } from 'react';

export const ImportModalContext = createContext();

const ImportModalContextProvider = props => {
  const [visible, setVisible] = useState(false);
  return (
    <ImportModalContext.Provider value={{ visible, setVisible }}>
      {props.children}
    </ImportModalContext.Provider>
  );
};
export default ImportModalContextProvider;
