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
-- Module      : Amazonka.AppFlow.Types.SlackConnectorProfileCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SlackConnectorProfileCredentials where

import Amazonka.AppFlow.Types.ConnectorOAuthRequest
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required when using Slack.
--
-- /See:/ 'newSlackConnectorProfileCredentials' smart constructor.
data SlackConnectorProfileCredentials = SlackConnectorProfileCredentials'
  { -- | The credentials used to access protected Slack resources.
    accessToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The OAuth requirement needed to request security tokens from the
    -- connector endpoint.
    oAuthRequest :: Prelude.Maybe ConnectorOAuthRequest,
    -- | The identifier for the client.
    clientId :: Prelude.Text,
    -- | The client secret used by the OAuth client to authenticate to the
    -- authorization server.
    clientSecret :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlackConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'slackConnectorProfileCredentials_accessToken' - The credentials used to access protected Slack resources.
--
-- 'oAuthRequest', 'slackConnectorProfileCredentials_oAuthRequest' - The OAuth requirement needed to request security tokens from the
-- connector endpoint.
--
-- 'clientId', 'slackConnectorProfileCredentials_clientId' - The identifier for the client.
--
-- 'clientSecret', 'slackConnectorProfileCredentials_clientSecret' - The client secret used by the OAuth client to authenticate to the
-- authorization server.
newSlackConnectorProfileCredentials ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'clientSecret'
  Prelude.Text ->
  SlackConnectorProfileCredentials
newSlackConnectorProfileCredentials
  pClientId_
  pClientSecret_ =
    SlackConnectorProfileCredentials'
      { accessToken =
          Prelude.Nothing,
        oAuthRequest = Prelude.Nothing,
        clientId = pClientId_,
        clientSecret =
          Core._Sensitive Lens.# pClientSecret_
      }

-- | The credentials used to access protected Slack resources.
slackConnectorProfileCredentials_accessToken :: Lens.Lens' SlackConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
slackConnectorProfileCredentials_accessToken = Lens.lens (\SlackConnectorProfileCredentials' {accessToken} -> accessToken) (\s@SlackConnectorProfileCredentials' {} a -> s {accessToken = a} :: SlackConnectorProfileCredentials) Prelude.. Lens.mapping Core._Sensitive

-- | The OAuth requirement needed to request security tokens from the
-- connector endpoint.
slackConnectorProfileCredentials_oAuthRequest :: Lens.Lens' SlackConnectorProfileCredentials (Prelude.Maybe ConnectorOAuthRequest)
slackConnectorProfileCredentials_oAuthRequest = Lens.lens (\SlackConnectorProfileCredentials' {oAuthRequest} -> oAuthRequest) (\s@SlackConnectorProfileCredentials' {} a -> s {oAuthRequest = a} :: SlackConnectorProfileCredentials)

-- | The identifier for the client.
slackConnectorProfileCredentials_clientId :: Lens.Lens' SlackConnectorProfileCredentials Prelude.Text
slackConnectorProfileCredentials_clientId = Lens.lens (\SlackConnectorProfileCredentials' {clientId} -> clientId) (\s@SlackConnectorProfileCredentials' {} a -> s {clientId = a} :: SlackConnectorProfileCredentials)

-- | The client secret used by the OAuth client to authenticate to the
-- authorization server.
slackConnectorProfileCredentials_clientSecret :: Lens.Lens' SlackConnectorProfileCredentials Prelude.Text
slackConnectorProfileCredentials_clientSecret = Lens.lens (\SlackConnectorProfileCredentials' {clientSecret} -> clientSecret) (\s@SlackConnectorProfileCredentials' {} a -> s {clientSecret = a} :: SlackConnectorProfileCredentials) Prelude.. Core._Sensitive

instance
  Prelude.Hashable
    SlackConnectorProfileCredentials
  where
  hashWithSalt
    salt'
    SlackConnectorProfileCredentials' {..} =
      salt' `Prelude.hashWithSalt` clientSecret
        `Prelude.hashWithSalt` clientId
        `Prelude.hashWithSalt` oAuthRequest
        `Prelude.hashWithSalt` accessToken

instance
  Prelude.NFData
    SlackConnectorProfileCredentials
  where
  rnf SlackConnectorProfileCredentials' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf oAuthRequest

instance Core.ToJSON SlackConnectorProfileCredentials where
  toJSON SlackConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("accessToken" Core..=) Prelude.<$> accessToken,
            ("oAuthRequest" Core..=) Prelude.<$> oAuthRequest,
            Prelude.Just ("clientId" Core..= clientId),
            Prelude.Just ("clientSecret" Core..= clientSecret)
          ]
      )
