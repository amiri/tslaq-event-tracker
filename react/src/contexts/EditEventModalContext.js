import React, { createContext, useState } from 'react';

export const EditEventModalContext = createContext();

const EditEventModalContextProvider = props => {
  const [visible, setVisible] = useState(false);
  return (
    <EditEventModalContext.Provider value={{ visible, setVisible }}>
      {props.children}
    </EditEventModalContext.Provider>
  );
};
export default EditEventModalContextProvider;
