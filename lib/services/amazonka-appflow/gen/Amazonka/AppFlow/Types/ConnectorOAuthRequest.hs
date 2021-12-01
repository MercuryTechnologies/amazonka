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
-- Module      : Amazonka.AppFlow.Types.ConnectorOAuthRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorOAuthRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Used by select connectors for which the OAuth workflow is supported,
-- such as Salesforce, Google Analytics, Marketo, Zendesk, and Slack.
--
-- /See:/ 'newConnectorOAuthRequest' smart constructor.
data ConnectorOAuthRequest = ConnectorOAuthRequest'
  { -- | The URL to which the authentication server redirects the browser after
    -- authorization has been granted.
    redirectUri :: Prelude.Maybe Prelude.Text,
    -- | The code provided by the connector when it has been authenticated via
    -- the connected app.
    authCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorOAuthRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redirectUri', 'connectorOAuthRequest_redirectUri' - The URL to which the authentication server redirects the browser after
-- authorization has been granted.
--
-- 'authCode', 'connectorOAuthRequest_authCode' - The code provided by the connector when it has been authenticated via
-- the connected app.
newConnectorOAuthRequest ::
  ConnectorOAuthRequest
newConnectorOAuthRequest =
  ConnectorOAuthRequest'
    { redirectUri =
        Prelude.Nothing,
      authCode = Prelude.Nothing
    }

-- | The URL to which the authentication server redirects the browser after
-- authorization has been granted.
connectorOAuthRequest_redirectUri :: Lens.Lens' ConnectorOAuthRequest (Prelude.Maybe Prelude.Text)
connectorOAuthRequest_redirectUri = Lens.lens (\ConnectorOAuthRequest' {redirectUri} -> redirectUri) (\s@ConnectorOAuthRequest' {} a -> s {redirectUri = a} :: ConnectorOAuthRequest)

-- | The code provided by the connector when it has been authenticated via
-- the connected app.
connectorOAuthRequest_authCode :: Lens.Lens' ConnectorOAuthRequest (Prelude.Maybe Prelude.Text)
connectorOAuthRequest_authCode = Lens.lens (\ConnectorOAuthRequest' {authCode} -> authCode) (\s@ConnectorOAuthRequest' {} a -> s {authCode = a} :: ConnectorOAuthRequest)

instance Prelude.Hashable ConnectorOAuthRequest where
  hashWithSalt salt' ConnectorOAuthRequest' {..} =
    salt' `Prelude.hashWithSalt` authCode
      `Prelude.hashWithSalt` redirectUri

instance Prelude.NFData ConnectorOAuthRequest where
  rnf ConnectorOAuthRequest' {..} =
    Prelude.rnf redirectUri
      `Prelude.seq` Prelude.rnf authCode

instance Core.ToJSON ConnectorOAuthRequest where
  toJSON ConnectorOAuthRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("redirectUri" Core..=) Prelude.<$> redirectUri,
            ("authCode" Core..=) Prelude.<$> authCode
          ]
      )
