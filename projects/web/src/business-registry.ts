import { AuthToken } from "./auth";

export interface Business {
    dataURL: string;
    gs1CompanyPrefix: string;
    name: string;
}

export class BusinessRegistry {
    constructor(private token: AuthToken) { }

    public getBusiness(): Promise<Business> {
        return new Promise((resolve, _) => {
            resolve({
                dataURL: "https://localhost:8200/",
                gs1CompanyPrefix: "930000",
                name: "We-Ship Ltd.",
            });
        });
    }
}
