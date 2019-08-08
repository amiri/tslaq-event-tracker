import React, { createContext, useReducer } from 'react';
import { authReducer } from '../reducers/AuthReducer';

export const AuthContext = createContext();

const user = JSON.parse(localStorage.getItem('user'));
const s = user ? { user } : {};

const AuthContextProvider = props => {
  const [state, dispatch] = useReducer(authReducer, s);

  // const register = registrationData => {
  //   window.api
  //     .postRegister(registrationData)
  //     .then(res => res.data)
  //     .then(u =>
  //       dispatch({
  //         type: 'REGISTER_SUCCESS',
  //         payload: u
  //       })
  //     )
  //     .catch(apiError => {
  //       logout();
  //       dispatch({
  //         type: 'REGISTER_FAILURE',
  //         payload: apiError
  //       });
  //     });
  // };
  return (
    <AuthContext.Provider value={{ ...state, dispatch }}>
      {props.children}
    </AuthContext.Provider>
  );
};

export default AuthContextProvider;
