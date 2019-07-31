import React, { createContext, useState } from 'react';

export const AuthContext = createContext();

const user = JSON.parse(localStorage.getItem('user'));
const s = user ? user : null;

const AuthContextProvider = props => {
  const [loggingIn, setLoggingIn] = useState(false);
  const [user, setUser] = useState(s);

  const login = async loginData => {
    setLoggingIn(true);
    await window.api
      .postLogin(loginData)
      .then(res => res.data)
      .then(u => {
        localStorage.setItem('user', JSON.stringify(u));
        setUser(u);
      })
      .then(setLoggingIn(false))
      .catch(error => {
        console.log(error);
      });
  };
  const logout = () => {
    window.api
      .getLogout()
      .then(res => res.data)
      .then(() => {
        localStorage.removeItem('user');
        setUser(null);
      });
  };
  return (
    <AuthContext.Provider
      value={{ loggingIn, setLoggingIn, login, logout, user }}
    >
      {props.children}
    </AuthContext.Provider>
  );
};

export default AuthContextProvider;
