import React, { createContext, useState } from 'react';

export const AuthModalContext = createContext();

const AuthModalContextProvider = props => {
  const [visible, setVisible] = useState(false);
  return (
    <AuthModalContext.Provider value={{ visible, setVisible }}>
      {props.children}
    </AuthModalContext.Provider>
  );
};
export default AuthModalContextProvider;
