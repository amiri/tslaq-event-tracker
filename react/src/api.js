import axios from 'axios';

class Api {
    constructor(jwt) {
        this.jwt = jwt;
    }

    getUsers() {
        return axios({
            url: '/users',
            baseURL: 'http://localhost:8888/',
            method: 'get',
            headers: {
                Authorization: 'Bearer ' + this.jwt,
                'Content-Type': 'application/json',
            },
        });
    }

    getUsersById(id) {
        return axios({
            url: String('/users/' + encodeURIComponent(id)),
            baseURL: 'http://localhost:8888/',
            method: 'get',
            headers: {
                Authorization: 'Bearer ' + this.jwt,
                'Content-Type': 'application/json',
            },
        });
    }

    postUsers(body) {
        return axios({
            url: '/users',
            baseURL: 'http://localhost:8888/',
            method: 'post',
            data: body,
            responseType: 'json',
            headers: {
                Authorization: 'Bearer ' + this.jwt,
                'Content-Type': 'application/json',
            },
        });
    }

    postEvents(body) {
        return axios({
            url: '/events',
            baseURL: 'http://localhost:8888/',
            method: 'post',
            data: body,
            responseType: 'json',
            headers: {
                Authorization: 'Bearer ' + this.jwt,
                'Content-Type': 'application/json',
            },
        });
    }

    getMetrics() {
        return axios({
            url: '/metrics',
            baseURL: 'http://localhost:8888/',
            method: 'get',
            headers: {
                Authorization: 'Bearer ' + this.jwt,
                'Content-Type': 'application/json',
            },
        });
    }

    getEvents() {
        return axios({
            url: '/events',
            baseURL: 'http://localhost:8888/',
            method: 'get',
        });
    }

    getEventsById(id) {
        return axios({
            url: String('/events/' + encodeURIComponent(id)),
            baseURL: 'http://localhost:8888/',
            method: 'get',
        });
    }

    postLogin(body) {
        return axios({
            url: '/login',
            baseURL: 'http://localhost:8888/',
            method: 'post',
            data: body,
            responseType: 'json',
            headers: { 'Content-Type': 'application/json' },
        });
    }

    getPrices() {
        return axios({
            url: '/prices',
            baseURL: 'http://localhost:8888/',
            method: 'get',
        });
    }
}
export { Api as default };
