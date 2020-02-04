import React, { createContext, useState } from 'react';

export const NewEventModalContext = createContext();

const NewEventModalContextProvider = props => {
  const [visible, setVisible] = useState(false);
  return (
    <NewEventModalContext.Provider value={{ visible, setVisible }}>
      {props.children}
    </NewEventModalContext.Provider>
  );
};
export default NewEventModalContextProvider;
