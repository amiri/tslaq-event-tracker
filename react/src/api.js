import axios from 'axios';

class Api {
    getUsers() {
        return axios({
            url: '/users',
            baseURL: 'http://localhost:8888/',
            method: 'get',
            withCredentials: true,
        });
    }

    getUsersById(id) {
        return axios({
            url: '/users/' + encodeURIComponent(id) + '',
            baseURL: 'http://localhost:8888/',
            method: 'get',
            withCredentials: true,
        });
    }

    postUsers(body) {
        return axios({
            url: '/users',
            baseURL: 'http://localhost:8888/',
            method: 'post',
            data: body,
            responseType: 'json',
            headers: { 'Content-Type': 'application/json' },
            withCredentials: true,
        });
    }

    postEvents(body) {
        return axios({
            url: '/events',
            baseURL: 'http://localhost:8888/',
            method: 'post',
            data: body,
            responseType: 'json',
            headers: { 'Content-Type': 'application/json' },
            withCredentials: true,
        });
    }

    getMetrics() {
        return axios({
            url: '/metrics',
            baseURL: 'http://localhost:8888/',
            method: 'get',
            withCredentials: true,
        });
    }

    getLogout() {
        return axios({
            url: '/logout',
            baseURL: 'http://localhost:8888/',
            method: 'get',
            withCredentials: true,
        });
    }

    getEvents() {
        return axios({
            url: '/events',
            baseURL: 'http://localhost:8888/',
            method: 'get',
            withCredentials: true,
        });
    }

    getEventsById(id) {
        return axios({
            url: '/events/' + encodeURIComponent(id) + '',
            baseURL: 'http://localhost:8888/',
            method: 'get',
            withCredentials: true,
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
            withCredentials: true,
        });
    }

    getPrices() {
        return axios({
            url: '/prices',
            baseURL: 'http://localhost:8888/',
            method: 'get',
            withCredentials: true,
        });
    }
}
export { Api as default };
