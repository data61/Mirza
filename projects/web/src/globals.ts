declare const OR_SERVICE_URL: string;

export const orUrl: string = OR_SERVICE_URL;

export const myGlobals = {
    edapiUrl: 'http://localhost:8020' as string,
    orUrl: OR_SERVICE_URL as string,
    googleMapsApiKey: process.env.GOOGLE_MAPS_API_KEY,
};
