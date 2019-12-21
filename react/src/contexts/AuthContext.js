import React, { createContext, useReducer } from 'react';
import { authReducer } from '../reducers/AuthReducer';

export const AuthContext = createContext();

const user = JSON.parse(localStorage.getItem('user'));
const s = user ? { user } : {};

const AuthContextProvider = props => {
  const [state, dispatch] = useReducer(authReducer, s);

  return (
    <AuthContext.Provider value={{ ...state, dispatch }}>
      {props.children}
    </AuthContext.Provider>
  );
};

export default AuthContextProvider;
