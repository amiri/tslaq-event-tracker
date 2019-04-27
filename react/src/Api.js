const getUsers = function(headerAuthorization, onSuccess, onError) {
    const xhr = new XMLHttpRequest();
    xhr.open('GET', '/users', true);
    xhr.setRequestHeader('Authorization', headerAuthorization);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onreadystatechange = function() {
        let res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            } else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onSuccess(res);
                }
            } else {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onError(res);
                }
            }
        }
    };

    xhr.send(null);
};

const getUsersById = function(id, headerAuthorization, onSuccess, onError) {
    const xhr = new XMLHttpRequest();
    xhr.open('GET', String('/users/' + encodeURIComponent(id)), true);
    xhr.setRequestHeader('Authorization', headerAuthorization);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onreadystatechange = function() {
        let res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            } else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onSuccess(res);
                }
            } else {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onError(res);
                }
            }
        }
    };

    xhr.send(null);
};

const postUsers = function(body, headerAuthorization, onSuccess, onError) {
    const xhr = new XMLHttpRequest();
    xhr.open('POST', '/users', true);
    xhr.setRequestHeader('Authorization', headerAuthorization);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onreadystatechange = function() {
        let res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            } else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onSuccess(res);
                }
            } else {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onError(res);
                }
            }
        }
    };

    xhr.send(JSON.stringify(body));
};

const postEvents = function(body, headerAuthorization, onSuccess, onError) {
    const xhr = new XMLHttpRequest();
    xhr.open('POST', '/events', true);
    xhr.setRequestHeader('Authorization', headerAuthorization);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onreadystatechange = function() {
        let res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            } else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onSuccess(res);
                }
            } else {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onError(res);
                }
            }
        }
    };

    xhr.send(JSON.stringify(body));
};

const getMetrics = function(headerAuthorization, onSuccess, onError) {
    const xhr = new XMLHttpRequest();
    xhr.open('GET', '/metrics', true);
    xhr.setRequestHeader('Authorization', headerAuthorization);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onreadystatechange = function() {
        let res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            } else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onSuccess(res);
                }
            } else {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onError(res);
                }
            }
        }
    };

    xhr.send(null);
};

const getEvents = function(onSuccess, onError) {
    const xhr = new XMLHttpRequest();
    xhr.open('GET', '/events', true);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onreadystatechange = function() {
        let res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            } else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onSuccess(res);
                }
            } else {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onError(res);
                }
            }
        }
    };

    xhr.send(null);
};

const getEventsById = function(id, onSuccess, onError) {
    const xhr = new XMLHttpRequest();
    xhr.open('GET', String('/events/' + encodeURIComponent(id)), true);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onreadystatechange = function() {
        let res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            } else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onSuccess(res);
                }
            } else {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onError(res);
                }
            }
        }
    };

    xhr.send(null);
};

const postLogin = function(body, onSuccess, onError) {
    const xhr = new XMLHttpRequest();
    xhr.open('POST', '/login', true);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onreadystatechange = function() {
        let res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            } else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onSuccess(res);
                }
            } else {
                try {
                    res = JSON.parse(xhr.responseText);
                } catch (error) {
                    onError(error);
                }

                if (res) {
                    onError(res);
                }
            }
        }
    };

    xhr.send(JSON.stringify(body));
};
