import * as auth0 from "auth0-js";
import * as Q from "query-string";

const webAuthOpts = {
    clientID: "JvrGZu2gNR84nrEOu1kEC3gxpcJn9wkU",
    domain: "mirza.au.auth0.com",
    redirectUri: window.location.origin,
    responseType: "token id_token",
    scope: "openid profile email",
};

const webAuth = new auth0.WebAuth(webAuthOpts);

export interface AuthToken extends auth0.Auth0DecodedHash {
    expires: number;
}

export class AuthState {
    private token: AuthToken;

    private tokenRenewalTimerID: number | null;
    constructor(token: AuthToken) {
        this.setToken(token);
    }

    public getToken(): AuthToken {
        return this.token;
    }
    public getName() {
        return this.token.idTokenPayload.name;
    }
    public getPictureUrl() {
        return this.token.idTokenPayload.picture;
    }
    private setToken(t: AuthToken): void {
        if (this.tokenRenewalTimerID !== null) {
            window.clearTimeout(this.tokenRenewalTimerID);
        }

        this.token = t;
        console.log(t);

        if (t.expires !== undefined) {
            // renew a minute before expiration
            const now = new Date();
            const delay = t.expires - now.getTime();
            this.tokenRenewalTimerID = window.setTimeout(() => {
                this.renewToken();
            }, delay);
        }
    }

    private renewToken() {
        webAuth.checkSession({}, (err, authResult) => {
            if (err !== null && err.error === "login_required") {
                logOut();
            } else if (authResult && authResult.accessToken && authResult.idToken) {
                this.setToken(setSession(authResult));
            }
        });
    }

}

function setSession(authResult: auth0.Auth0DecodedHash): AuthToken {
    const result = authResult as AuthToken;

    if (authResult.expiresIn !== undefined) {
        result.expires = (authResult.expiresIn * 1000) + new Date().getTime();
    }

    localStorage.setItem("auth0_tk", JSON.stringify(result));

    return result;
}

function getSession(): AuthToken | null {
    const s = localStorage.getItem("auth0_tk");
    if (s !== null) {
        return JSON.parse(s);
    }
    return null;
}

function clearSession(): void {
    localStorage.removeItem("auth0_tk");
}

function expired(tk: AuthToken): boolean {
    if (tk.expires !== undefined && Number.isInteger(tk.expires)) {
        return (new Date()).getTime() > tk.expires;
    }
    return true;
}

export function authInit(): Promise<AuthState | null> {
    // Either:
    // - has query Params from an auth redirect, store and return
    // - has previous (non-expired) session, return it in a new AuthState
    // - has no previous session, or previous has expired - redirect to Auth0
    return new Promise( (resolve, reject) => {
        // if we have a prev session then we assume we aren't coming back from an auth request...
        const prev = getSession();

        if (prev !== null) {
            if (expired(prev)) {
                clearSession();
                resolve(null);
            } else {
                resolve(new AuthState(prev));
            }
        } else {
            const h = Q.parse(window.location.hash);
            if (h.access_token) {
                webAuth.parseHash( (err, result) => {
                    if (result && result.accessToken && result.idToken) {
                        const tk = setSession(result);
                        window.location.hash = "";
                        resolve(new AuthState(tk));

                    } else if (err) {
                        reject(err);
                    }
                });
            } else {
                resolve(null);
            }
        }
    });
}

export function logOut() {
    clearSession();
    window.location.reload(false);
}
export function logIn() {
    clearSession();
    webAuth.authorize();
}
