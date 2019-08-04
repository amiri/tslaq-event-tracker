import React, { createContext, useState } from 'react';

export const AuthContext = createContext();

const user = JSON.parse(localStorage.getItem('user'));
const s = user ? { isSubmitting: false, user } : { isSubmitting: false };

const reducer = ({ state, action }) => {
  switch (action.type) {
    case 'LOGIN_ATTEMPT':
      return {
        ...state,
        isSubmitting: true
      };
    case 'LOGIN_SUCCESS':
      localStorage.setItem('user', JSON.stringify(action.payload));
      return {
        ...state,
        isSubmitting: false,
        user
      };
    case 'LOGIN_FAILURE':
      return {
        ...state,
        isSubmitting: false,
        apiError: action.payload
      };
    case 'LOGOUT':
      localStorage.removeItem('user');
      const { user, ...noUser } = state;
      return {
        ...noUser,
        isSubmitting: false
      };
    case 'REGISTER_SUCCESS':
      localStorage.setItem('user', JSON.stringify(action.payload));
      return {
        ...state,
        isSubmitting: false,
        user
      };
    case 'REGISTER_FAILURE':
      return {
        ...state,
        isSubmitting: false,
        apiError: action.payload
      };
    default:
      return state;
  }
};

const AuthContextProvider = props => {
  const [state, dispatch] = useReducer(s);

  const login = async loginData => {
    await window.api
      .postLogin(loginData)
      .then(res => res.data)
      .then(u =>
        dispatch({
          type: LOGIN_SUCCESS,
          payload: u
        })
      )
      .catch(apiError => {
        logout();
        dispatch({
          type: LOGIN_FAILURE,
          payload: apiError
        });
      });
  };
  const logout = () => {
    window.api
      .getLogout()
      .then(res => res.data)
      .then(data =>
        dispatch({
          type: LOGOUT,
          payload: data
        })
      );
  };
  const register = registrationData => {
    window.api
      .postRegister()
      .then(res => res.data)
      .then(u =>
        dispatch({
          type: REGISTER_SUCCESS,
          payload: u
        })
      )
      .catch(apiError => {
        logout();
        dispatch({
          type: REGISTER_FAILURE,
          payload: apiError
        });
      });
  };
  return (
    <AuthContext.Provider
      value={{ isSubmitting, login, logout, register, user }}
    >
      {props.children}
    </AuthContext.Provider>
  );
};

export default AuthContextProvider;
