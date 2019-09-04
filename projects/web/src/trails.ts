import { string } from "prop-types";

export interface UnsignedTrailEntry {
  version: number;
  timestamp: string;
  org: string;
  event_id:string;
  previous_signatures : string[];
}

export interface SignedTrailEntry extends UnsignedTrailEntry {
  signature: string;
}
