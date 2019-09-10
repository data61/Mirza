import { string } from "prop-types";
import { isArray } from "util";

export interface SignedTrailEntry extends UnsignedTrailEntry {
  signature: string;
}

export interface UnsignedTrailEntry {
  version: number;
  timestamp: string;
  org: string;
  event_id:string;
  previous_signatures : string[];
}

// Its a real shame that we need to write this explicitly and effectively have to duplicate the type. It would be nice
// to be able to derrive this directly from the type and if this is possible this should be replaced with that
// implementation.
export function isSignedTrailEntryArray(value: any): value is SignedTrailEntry[] {
  if (isArray(value)) {
    return value.every(item => isSignedTrailEntry(item));
  }

  return false;
}

// Its a real shame that we need to write this explicitly and effectively have to duplicate the type. It would be nice
// to be able to derrive this directly from the type and if this is possible this should be replaced with that
// implementation.
function isSignedTrailEntry(value: any): value is SignedTrailEntry {
  if (value as SignedTrailEntry) {
    if (("signature" in value) &&
        (typeof value.signature === "string")) {
      return isUnsignedTrailEntry(value);
    }
  }

  return false;
}

// Its a real shame that we need to write this explicitly and effectively have to duplicate the type. It would be nice
// to be able to derrive this directly from the type and if this is possible this should be replaced with that
// implementation.
function isUnsignedTrailEntry(value: any): value is UnsignedTrailEntry {
  if (value as UnsignedTrailEntry){
    if (("version" in value) &&
        (typeof value.version === "number") &&
        ("timestamp" in value) &&
        (typeof value.timestamp === "string") &&
        ("org" in value) &&
        (typeof value.org === "string") &&
        ("event_id" in value) &&
        (typeof value.event_id === "string") &&
        ("previous_signatures" in value) &&
        (isArray(value.previous_signatures)) &&
        (value.previous_signatures.every((signature: any) => typeof signature === "string"))) {
      return true;
    }
  }

  return false;
}
