import axios from 'axios';

class Api {
    constructor(jwt) {
        this.jwt = jwt;
    }

    getUsers() {
        return axios({
            url: '/users',
            method: 'get',
            headers: { Authorization: 'Bearer ' + this.jwt },
        });
    }

    getUsersById(id) {
        return axios({
            url: String('/users/' + encodeURIComponent(id)),
            method: 'get',
            headers: { Authorization: 'Bearer ' + this.jwt },
        });
    }

    postUsers(body) {
        return axios({
            url: '/users',
            method: 'post',
            data: body,
            responseType: 'json',
            headers: { Authorization: 'Bearer ' + this.jwt },
        });
    }

    postEvents(body) {
        return axios({
            url: '/events',
            method: 'post',
            data: body,
            responseType: 'json',
            headers: { Authorization: 'Bearer ' + this.jwt },
        });
    }

    getMetrics() {
        return axios({
            url: '/metrics',
            method: 'get',
            headers: { Authorization: 'Bearer ' + this.jwt },
        });
    }

    getEvents() {
        return axios({ url: '/events', method: 'get' });
    }

    getEventsById(id) {
        return axios({
            url: String('/events/' + encodeURIComponent(id)),
            method: 'get',
        });
    }

    postLogin(body) {
        return axios({
            url: '/login',
            method: 'post',
            data: body,
            responseType: 'json',
        });
    }

    getPrices() {
        return axios({ url: '/prices', method: 'get' });
    }
}
export { Api as default };
