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
-- Module      : Amazonka.AmplifyBackend.Types.UpdateBackendAuthOAuthConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.UpdateBackendAuthOAuthConfig where

import Amazonka.AmplifyBackend.Types.OAuthGrantType
import Amazonka.AmplifyBackend.Types.OAuthScopesElement
import Amazonka.AmplifyBackend.Types.SocialProviderSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The OAuth configurations for authenticating users into your Amplify app.
--
-- /See:/ 'newUpdateBackendAuthOAuthConfig' smart constructor.
data UpdateBackendAuthOAuthConfig = UpdateBackendAuthOAuthConfig'
  { -- | Describes third-party social federation configurations for allowing your
    -- users to sign in with OAuth.
    socialProviderSettings :: Prelude.Maybe SocialProviderSettings,
    -- | The Amazon Cognito domain prefix used to create a hosted UI for
    -- authentication.
    domainPrefix :: Prelude.Maybe Prelude.Text,
    -- | The list of OAuth-related flows that can allow users to authenticate
    -- from your Amplify app.
    oAuthScopes :: Prelude.Maybe [OAuthScopesElement],
    -- | The OAuth grant type to allow app users to authenticate from your
    -- Amplify app.
    oAuthGrantType :: Prelude.Maybe OAuthGrantType,
    -- | Redirect URLs that OAuth uses when a user signs out of an Amplify app.
    redirectSignOutURIs :: Prelude.Maybe [Prelude.Text],
    -- | Redirect URLs that OAuth uses when a user signs in to an Amplify app.
    redirectSignInURIs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAuthOAuthConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'socialProviderSettings', 'updateBackendAuthOAuthConfig_socialProviderSettings' - Describes third-party social federation configurations for allowing your
-- users to sign in with OAuth.
--
-- 'domainPrefix', 'updateBackendAuthOAuthConfig_domainPrefix' - The Amazon Cognito domain prefix used to create a hosted UI for
-- authentication.
--
-- 'oAuthScopes', 'updateBackendAuthOAuthConfig_oAuthScopes' - The list of OAuth-related flows that can allow users to authenticate
-- from your Amplify app.
--
-- 'oAuthGrantType', 'updateBackendAuthOAuthConfig_oAuthGrantType' - The OAuth grant type to allow app users to authenticate from your
-- Amplify app.
--
-- 'redirectSignOutURIs', 'updateBackendAuthOAuthConfig_redirectSignOutURIs' - Redirect URLs that OAuth uses when a user signs out of an Amplify app.
--
-- 'redirectSignInURIs', 'updateBackendAuthOAuthConfig_redirectSignInURIs' - Redirect URLs that OAuth uses when a user signs in to an Amplify app.
newUpdateBackendAuthOAuthConfig ::
  UpdateBackendAuthOAuthConfig
newUpdateBackendAuthOAuthConfig =
  UpdateBackendAuthOAuthConfig'
    { socialProviderSettings =
        Prelude.Nothing,
      domainPrefix = Prelude.Nothing,
      oAuthScopes = Prelude.Nothing,
      oAuthGrantType = Prelude.Nothing,
      redirectSignOutURIs = Prelude.Nothing,
      redirectSignInURIs = Prelude.Nothing
    }

-- | Describes third-party social federation configurations for allowing your
-- users to sign in with OAuth.
updateBackendAuthOAuthConfig_socialProviderSettings :: Lens.Lens' UpdateBackendAuthOAuthConfig (Prelude.Maybe SocialProviderSettings)
updateBackendAuthOAuthConfig_socialProviderSettings = Lens.lens (\UpdateBackendAuthOAuthConfig' {socialProviderSettings} -> socialProviderSettings) (\s@UpdateBackendAuthOAuthConfig' {} a -> s {socialProviderSettings = a} :: UpdateBackendAuthOAuthConfig)

-- | The Amazon Cognito domain prefix used to create a hosted UI for
-- authentication.
updateBackendAuthOAuthConfig_domainPrefix :: Lens.Lens' UpdateBackendAuthOAuthConfig (Prelude.Maybe Prelude.Text)
updateBackendAuthOAuthConfig_domainPrefix = Lens.lens (\UpdateBackendAuthOAuthConfig' {domainPrefix} -> domainPrefix) (\s@UpdateBackendAuthOAuthConfig' {} a -> s {domainPrefix = a} :: UpdateBackendAuthOAuthConfig)

-- | The list of OAuth-related flows that can allow users to authenticate
-- from your Amplify app.
updateBackendAuthOAuthConfig_oAuthScopes :: Lens.Lens' UpdateBackendAuthOAuthConfig (Prelude.Maybe [OAuthScopesElement])
updateBackendAuthOAuthConfig_oAuthScopes = Lens.lens (\UpdateBackendAuthOAuthConfig' {oAuthScopes} -> oAuthScopes) (\s@UpdateBackendAuthOAuthConfig' {} a -> s {oAuthScopes = a} :: UpdateBackendAuthOAuthConfig) Prelude.. Lens.mapping Lens.coerced

-- | The OAuth grant type to allow app users to authenticate from your
-- Amplify app.
updateBackendAuthOAuthConfig_oAuthGrantType :: Lens.Lens' UpdateBackendAuthOAuthConfig (Prelude.Maybe OAuthGrantType)
updateBackendAuthOAuthConfig_oAuthGrantType = Lens.lens (\UpdateBackendAuthOAuthConfig' {oAuthGrantType} -> oAuthGrantType) (\s@UpdateBackendAuthOAuthConfig' {} a -> s {oAuthGrantType = a} :: UpdateBackendAuthOAuthConfig)

-- | Redirect URLs that OAuth uses when a user signs out of an Amplify app.
updateBackendAuthOAuthConfig_redirectSignOutURIs :: Lens.Lens' UpdateBackendAuthOAuthConfig (Prelude.Maybe [Prelude.Text])
updateBackendAuthOAuthConfig_redirectSignOutURIs = Lens.lens (\UpdateBackendAuthOAuthConfig' {redirectSignOutURIs} -> redirectSignOutURIs) (\s@UpdateBackendAuthOAuthConfig' {} a -> s {redirectSignOutURIs = a} :: UpdateBackendAuthOAuthConfig) Prelude.. Lens.mapping Lens.coerced

-- | Redirect URLs that OAuth uses when a user signs in to an Amplify app.
updateBackendAuthOAuthConfig_redirectSignInURIs :: Lens.Lens' UpdateBackendAuthOAuthConfig (Prelude.Maybe [Prelude.Text])
updateBackendAuthOAuthConfig_redirectSignInURIs = Lens.lens (\UpdateBackendAuthOAuthConfig' {redirectSignInURIs} -> redirectSignInURIs) (\s@UpdateBackendAuthOAuthConfig' {} a -> s {redirectSignInURIs = a} :: UpdateBackendAuthOAuthConfig) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    UpdateBackendAuthOAuthConfig
  where
  hashWithSalt salt' UpdateBackendAuthOAuthConfig' {..} =
    salt' `Prelude.hashWithSalt` redirectSignInURIs
      `Prelude.hashWithSalt` redirectSignOutURIs
      `Prelude.hashWithSalt` oAuthGrantType
      `Prelude.hashWithSalt` oAuthScopes
      `Prelude.hashWithSalt` domainPrefix
      `Prelude.hashWithSalt` socialProviderSettings

instance Prelude.NFData UpdateBackendAuthOAuthConfig where
  rnf UpdateBackendAuthOAuthConfig' {..} =
    Prelude.rnf socialProviderSettings
      `Prelude.seq` Prelude.rnf redirectSignInURIs
      `Prelude.seq` Prelude.rnf redirectSignOutURIs
      `Prelude.seq` Prelude.rnf oAuthGrantType
      `Prelude.seq` Prelude.rnf oAuthScopes
      `Prelude.seq` Prelude.rnf domainPrefix

instance Core.ToJSON UpdateBackendAuthOAuthConfig where
  toJSON UpdateBackendAuthOAuthConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("socialProviderSettings" Core..=)
              Prelude.<$> socialProviderSettings,
            ("domainPrefix" Core..=) Prelude.<$> domainPrefix,
            ("oAuthScopes" Core..=) Prelude.<$> oAuthScopes,
            ("oAuthGrantType" Core..=)
              Prelude.<$> oAuthGrantType,
            ("redirectSignOutURIs" Core..=)
              Prelude.<$> redirectSignOutURIs,
            ("redirectSignInURIs" Core..=)
              Prelude.<$> redirectSignInURIs
          ]
      )
