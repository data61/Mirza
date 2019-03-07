import * as React from 'react';

// tslint:disable-next-line: no-empty-interface
export interface FooterProps { }

export class Footer extends React.Component<FooterProps, {}> {
  public render() {
    return <footer>
      <div className='container align-center'>Copyright &copy; 2019 CSIRO Data61</div>
    </footer>;
  }
}
