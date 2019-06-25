import { AuthToken } from "./auth";
import { orUrl } from "./globals";

export interface Organisation {
    url: string;
    companyPrefix: string;
    name: string;
}

export class BusinessRegistry {
    constructor(private token: AuthToken) { }

    public getOrganisations(): Promise<Organisation[]> {
        console.log("Using OR at: " + orUrl);

        return fetch(orUrl + '/user/orgs', {
            method: 'GET',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json',
                'Authorization': 'Bearer ' + this.token.idToken,
            },
            credentials: 'include',
        }).then((res: Response) => {
            if (res.status === 401 && confirm("First time?")) {
                return this.tryRegister();
            } else if (res.status === 200) {
                return res.json();
            }
        }).then((body: any[]) => body.map((x) => ({
            url: x.url,
            companyPrefix: x.company_prefix,
            name: x.name,
        })));
    }

    private async tryRegister(): Promise<boolean> {
        return fetch(orUrl + '/user', {
            method: 'PUT',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json',
                'Authorization': 'Bearer ' + this.token.idToken,
            },
            credentials: 'include',
        }).then((res: Response) => (res.status === 200));
    }
}
