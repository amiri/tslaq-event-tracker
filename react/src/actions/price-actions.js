import Api from '../api';
import { GET_PRICES } from './types';

const fetch = require('node-fetch');

export const getPrices = () => dispatch => {
    window.api = new Api();
    window.api
        .getPrices()
        .then(res => res.data)
        .then(data =>
            fetch(data.url)
                .then(res => res.json())
                .then(data =>
                    dispatch({
                        type: GET_PRICES,
                        payload: data.prices,
                    }),
                ),
        );
};
