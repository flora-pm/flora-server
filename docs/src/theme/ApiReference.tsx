import React from 'react';
import Layout from '@theme/Layout';
import { RedocStandalone } from 'redoc';

export default function ApiReference({specUrl}) {
    // makes the fixed sidebar and scrolling play nicey with docusaurus navbar
    const navbarHeight = () => (document.querySelector('.navbar') as HTMLElement).offsetHeight;
    return (
        <Layout>
            <main>
                <RedocStandalone
                    specUrl={specUrl}
                    options={{
                        scrollYOffset: navbarHeight,
                        theme: {
                            colors: {
                                primary: {
                                    main: '#25c2a0'
                                },
                                error: {
                                    main: '#f2484b'
                                },
                                success: {
                                    main: '#26a149'
                                }
                            },
                            typography: {
                                fontSize: 'var(--ifm-font-size-base)',
                                lineHeight: 'var(--ifm-line-height-base)',
                                fontFamily: 'var(--ifm-font-family-base)',
                                headings: {
                                    fontFamily: 'var(--ifm-font-family-base)',
                                    fontWeight: 'var(--ifm-heading-font-weight)'
                                },
                                code: {
                                    lineHeight: 'var(--ifm-pre-line-height)',
                                    fontFamily: 'var(--ifm-font-family-monospace)',
                                    color: 'var(--redoc-code-color)',
                                    backgroundColor: 'var(--redoc-code-background-color)'
                                }
                            },
                            schema: {
                                requireLabelColor: 'var(--redoc-require-label-color)'
                            },
                            // about the same as the sidebar in the docs area, for consistency
                            sidebar: {
                                width: '300px'
                            }
                        }
                    }}
                />
            </main>
        </Layout>
    );
}
