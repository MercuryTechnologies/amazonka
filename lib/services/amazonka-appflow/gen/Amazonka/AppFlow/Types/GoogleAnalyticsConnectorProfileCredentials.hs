{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppFlow.Types.GoogleAnalyticsConnectorProfileCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.GoogleAnalyticsConnectorProfileCredentials where

import Amazonka.AppFlow.Types.ConnectorOAuthRequest
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required by Google Analytics.
--
-- /See:/ 'newGoogleAnalyticsConnectorProfileCredentials' smart constructor.
data GoogleAnalyticsConnectorProfileCredentials = GoogleAnalyticsConnectorProfileCredentials'
  { -- | The credentials used to access protected Google Analytics resources.
    accessToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The credentials used to acquire new access tokens. This is required only
    -- for OAuth2 access tokens, and is not required for OAuth1 access tokens.
    refreshToken :: Prelude.Maybe Prelude.Text,
    -- | The OAuth requirement needed to request security tokens from the
    -- connector endpoint.
    oAuthRequest :: Prelude.Maybe ConnectorOAuthRequest,
    -- | The identifier for the desired client.
    clientId :: Prelude.Text,
    -- | The client secret used by the OAuth client to authenticate to the
    -- authorization server.
    clientSecret :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GoogleAnalyticsConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'googleAnalyticsConnectorProfileCredentials_accessToken' - The credentials used to access protected Google Analytics resources.
--
-- 'refreshToken', 'googleAnalyticsConnectorProfileCredentials_refreshToken' - The credentials used to acquire new access tokens. This is required only
-- for OAuth2 access tokens, and is not required for OAuth1 access tokens.
--
-- 'oAuthRequest', 'googleAnalyticsConnectorProfileCredentials_oAuthRequest' - The OAuth requirement needed to request security tokens from the
-- connector endpoint.
--
-- 'clientId', 'googleAnalyticsConnectorProfileCredentials_clientId' - The identifier for the desired client.
--
-- 'clientSecret', 'googleAnalyticsConnectorProfileCredentials_clientSecret' - The client secret used by the OAuth client to authenticate to the
-- authorization server.
newGoogleAnalyticsConnectorProfileCredentials ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'clientSecret'
  Prelude.Text ->
  GoogleAnalyticsConnectorProfileCredentials
newGoogleAnalyticsConnectorProfileCredentials
  pClientId_
  pClientSecret_ =
    GoogleAnalyticsConnectorProfileCredentials'
      { accessToken =
          Prelude.Nothing,
        refreshToken = Prelude.Nothing,
        oAuthRequest = Prelude.Nothing,
        clientId = pClientId_,
        clientSecret =
          Core._Sensitive
            Lens.# pClientSecret_
      }

-- | The credentials used to access protected Google Analytics resources.
googleAnalyticsConnectorProfileCredentials_accessToken :: Lens.Lens' GoogleAnalyticsConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
googleAnalyticsConnectorProfileCredentials_accessToken = Lens.lens (\GoogleAnalyticsConnectorProfileCredentials' {accessToken} -> accessToken) (\s@GoogleAnalyticsConnectorProfileCredentials' {} a -> s {accessToken = a} :: GoogleAnalyticsConnectorProfileCredentials) Prelude.. Lens.mapping Core._Sensitive

-- | The credentials used to acquire new access tokens. This is required only
-- for OAuth2 access tokens, and is not required for OAuth1 access tokens.
googleAnalyticsConnectorProfileCredentials_refreshToken :: Lens.Lens' GoogleAnalyticsConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
googleAnalyticsConnectorProfileCredentials_refreshToken = Lens.lens (\GoogleAnalyticsConnectorProfileCredentials' {refreshToken} -> refreshToken) (\s@GoogleAnalyticsConnectorProfileCredentials' {} a -> s {refreshToken = a} :: GoogleAnalyticsConnectorProfileCredentials)

-- | The OAuth requirement needed to request security tokens from the
-- connector endpoint.
googleAnalyticsConnectorProfileCredentials_oAuthRequest :: Lens.Lens' GoogleAnalyticsConnectorProfileCredentials (Prelude.Maybe ConnectorOAuthRequest)
googleAnalyticsConnectorProfileCredentials_oAuthRequest = Lens.lens (\GoogleAnalyticsConnectorProfileCredentials' {oAuthRequest} -> oAuthRequest) (\s@GoogleAnalyticsConnectorProfileCredentials' {} a -> s {oAuthRequest = a} :: GoogleAnalyticsConnectorProfileCredentials)

-- | The identifier for the desired client.
googleAnalyticsConnectorProfileCredentials_clientId :: Lens.Lens' GoogleAnalyticsConnectorProfileCredentials Prelude.Text
googleAnalyticsConnectorProfileCredentials_clientId = Lens.lens (\GoogleAnalyticsConnectorProfileCredentials' {clientId} -> clientId) (\s@GoogleAnalyticsConnectorProfileCredentials' {} a -> s {clientId = a} :: GoogleAnalyticsConnectorProfileCredentials)

-- | The client secret used by the OAuth client to authenticate to the
-- authorization server.
googleAnalyticsConnectorProfileCredentials_clientSecret :: Lens.Lens' GoogleAnalyticsConnectorProfileCredentials Prelude.Text
googleAnalyticsConnectorProfileCredentials_clientSecret = Lens.lens (\GoogleAnalyticsConnectorProfileCredentials' {clientSecret} -> clientSecret) (\s@GoogleAnalyticsConnectorProfileCredentials' {} a -> s {clientSecret = a} :: GoogleAnalyticsConnectorProfileCredentials) Prelude.. Core._Sensitive

instance
  Prelude.Hashable
    GoogleAnalyticsConnectorProfileCredentials
  where
  hashWithSalt
    salt'
    GoogleAnalyticsConnectorProfileCredentials' {..} =
      salt' `Prelude.hashWithSalt` clientSecret
        `Prelude.hashWithSalt` clientId
        `Prelude.hashWithSalt` oAuthRequest
        `Prelude.hashWithSalt` refreshToken
        `Prelude.hashWithSalt` accessToken

instance
  Prelude.NFData
    GoogleAnalyticsConnectorProfileCredentials
  where
  rnf GoogleAnalyticsConnectorProfileCredentials' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf oAuthRequest
      `Prelude.seq` Prelude.rnf refreshToken

instance
  Core.ToJSON
    GoogleAnalyticsConnectorProfileCredentials
  where
  toJSON
    GoogleAnalyticsConnectorProfileCredentials' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("accessToken" Core..=) Prelude.<$> accessToken,
              ("refreshToken" Core..=) Prelude.<$> refreshToken,
              ("oAuthRequest" Core..=) Prelude.<$> oAuthRequest,
              Prelude.Just ("clientId" Core..= clientId),
              Prelude.Just ("clientSecret" Core..= clientSecret)
            ]
        )
