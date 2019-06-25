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

        //     .then(function(res: Response) {
        //     return res.json();
        // }).then(function(data) {
        //     if (data[0]) {
        //         return data[0].url;
        //     }
        //     return Promise.resolve();
        // }).then(function(url) {
        //     const [event, _] = eventState;
        //     const dl = DigitalLink(event.epcList[0]);
        //     if (!dl.isValid()) {
        //         return Promise.reject("Invalid GS1 Label");
        //     }
        //     event.epcList[0] = dl.mapToGS1Urn();
        //     console.log(dl.mapToGS1Urn());
        //     console.log(JSON.stringify(event));
        //     const request = new Request(url + '/event', {
        //         method: 'POST',
        //         body: JSON.stringify(event),
        //         headers: new Headers({
        //             'Accept': 'application/json',
        //             'Content-Type': 'application/json',
        //             'Authorization': token,
        //         }),
        //         credentials: 'include',
        //     });
        //     return fetch(request);
        // }).then(function(res: Response) {
        //     if (res.status === 200) {
        //         alert('Success!');
        //     } else {
        //         alert('Failed with status: ' + res.status);
        //     }
        // }).catch(function(err) {
        //     console.log(err);
        // });

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
