import React, { createContext, useState } from 'react';

export const NewCategoryModalContext = createContext();

const NewCategoryModalContextProvider = props => {
  const [visible, setVisible] = useState(false);
  return (
    <NewCategoryModalContext.Provider value={{ visible, setVisible }}>
      {props.children}
    </NewCategoryModalContext.Provider>
  );
};
export default NewCategoryModalContextProvider;
