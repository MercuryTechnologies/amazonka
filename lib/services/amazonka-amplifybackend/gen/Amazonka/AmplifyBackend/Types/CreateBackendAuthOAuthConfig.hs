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
-- Module      : Amazonka.AmplifyBackend.Types.CreateBackendAuthOAuthConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.CreateBackendAuthOAuthConfig where

import Amazonka.AmplifyBackend.Types.OAuthGrantType
import Amazonka.AmplifyBackend.Types.OAuthScopesElement
import Amazonka.AmplifyBackend.Types.SocialProviderSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Creates the OAuth configuration for your Amplify project.
--
-- /See:/ 'newCreateBackendAuthOAuthConfig' smart constructor.
data CreateBackendAuthOAuthConfig = CreateBackendAuthOAuthConfig'
  { -- | The settings for using social providers to access your Amplify app.
    socialProviderSettings :: Prelude.Maybe SocialProviderSettings,
    -- | The domain prefix for your Amplify app.
    domainPrefix :: Prelude.Maybe Prelude.Text,
    -- | Redirect URLs that OAuth uses when a user signs out of an Amplify app.
    redirectSignOutURIs :: [Prelude.Text],
    -- | The redirected URI for signing in to your Amplify app.
    redirectSignInURIs :: [Prelude.Text],
    -- | The OAuth grant type that you use to allow app users to authenticate
    -- from your Amplify app.
    oAuthGrantType :: OAuthGrantType,
    -- | List of OAuth-related flows used to allow your app users to authenticate
    -- from your Amplify app.
    oAuthScopes :: [OAuthScopesElement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendAuthOAuthConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'socialProviderSettings', 'createBackendAuthOAuthConfig_socialProviderSettings' - The settings for using social providers to access your Amplify app.
--
-- 'domainPrefix', 'createBackendAuthOAuthConfig_domainPrefix' - The domain prefix for your Amplify app.
--
-- 'redirectSignOutURIs', 'createBackendAuthOAuthConfig_redirectSignOutURIs' - Redirect URLs that OAuth uses when a user signs out of an Amplify app.
--
-- 'redirectSignInURIs', 'createBackendAuthOAuthConfig_redirectSignInURIs' - The redirected URI for signing in to your Amplify app.
--
-- 'oAuthGrantType', 'createBackendAuthOAuthConfig_oAuthGrantType' - The OAuth grant type that you use to allow app users to authenticate
-- from your Amplify app.
--
-- 'oAuthScopes', 'createBackendAuthOAuthConfig_oAuthScopes' - List of OAuth-related flows used to allow your app users to authenticate
-- from your Amplify app.
newCreateBackendAuthOAuthConfig ::
  -- | 'oAuthGrantType'
  OAuthGrantType ->
  CreateBackendAuthOAuthConfig
newCreateBackendAuthOAuthConfig pOAuthGrantType_ =
  CreateBackendAuthOAuthConfig'
    { socialProviderSettings =
        Prelude.Nothing,
      domainPrefix = Prelude.Nothing,
      redirectSignOutURIs = Prelude.mempty,
      redirectSignInURIs = Prelude.mempty,
      oAuthGrantType = pOAuthGrantType_,
      oAuthScopes = Prelude.mempty
    }

-- | The settings for using social providers to access your Amplify app.
createBackendAuthOAuthConfig_socialProviderSettings :: Lens.Lens' CreateBackendAuthOAuthConfig (Prelude.Maybe SocialProviderSettings)
createBackendAuthOAuthConfig_socialProviderSettings = Lens.lens (\CreateBackendAuthOAuthConfig' {socialProviderSettings} -> socialProviderSettings) (\s@CreateBackendAuthOAuthConfig' {} a -> s {socialProviderSettings = a} :: CreateBackendAuthOAuthConfig)

-- | The domain prefix for your Amplify app.
createBackendAuthOAuthConfig_domainPrefix :: Lens.Lens' CreateBackendAuthOAuthConfig (Prelude.Maybe Prelude.Text)
createBackendAuthOAuthConfig_domainPrefix = Lens.lens (\CreateBackendAuthOAuthConfig' {domainPrefix} -> domainPrefix) (\s@CreateBackendAuthOAuthConfig' {} a -> s {domainPrefix = a} :: CreateBackendAuthOAuthConfig)

-- | Redirect URLs that OAuth uses when a user signs out of an Amplify app.
createBackendAuthOAuthConfig_redirectSignOutURIs :: Lens.Lens' CreateBackendAuthOAuthConfig [Prelude.Text]
createBackendAuthOAuthConfig_redirectSignOutURIs = Lens.lens (\CreateBackendAuthOAuthConfig' {redirectSignOutURIs} -> redirectSignOutURIs) (\s@CreateBackendAuthOAuthConfig' {} a -> s {redirectSignOutURIs = a} :: CreateBackendAuthOAuthConfig) Prelude.. Lens.coerced

-- | The redirected URI for signing in to your Amplify app.
createBackendAuthOAuthConfig_redirectSignInURIs :: Lens.Lens' CreateBackendAuthOAuthConfig [Prelude.Text]
createBackendAuthOAuthConfig_redirectSignInURIs = Lens.lens (\CreateBackendAuthOAuthConfig' {redirectSignInURIs} -> redirectSignInURIs) (\s@CreateBackendAuthOAuthConfig' {} a -> s {redirectSignInURIs = a} :: CreateBackendAuthOAuthConfig) Prelude.. Lens.coerced

-- | The OAuth grant type that you use to allow app users to authenticate
-- from your Amplify app.
createBackendAuthOAuthConfig_oAuthGrantType :: Lens.Lens' CreateBackendAuthOAuthConfig OAuthGrantType
createBackendAuthOAuthConfig_oAuthGrantType = Lens.lens (\CreateBackendAuthOAuthConfig' {oAuthGrantType} -> oAuthGrantType) (\s@CreateBackendAuthOAuthConfig' {} a -> s {oAuthGrantType = a} :: CreateBackendAuthOAuthConfig)

-- | List of OAuth-related flows used to allow your app users to authenticate
-- from your Amplify app.
createBackendAuthOAuthConfig_oAuthScopes :: Lens.Lens' CreateBackendAuthOAuthConfig [OAuthScopesElement]
createBackendAuthOAuthConfig_oAuthScopes = Lens.lens (\CreateBackendAuthOAuthConfig' {oAuthScopes} -> oAuthScopes) (\s@CreateBackendAuthOAuthConfig' {} a -> s {oAuthScopes = a} :: CreateBackendAuthOAuthConfig) Prelude.. Lens.coerced

instance Core.FromJSON CreateBackendAuthOAuthConfig where
  parseJSON =
    Core.withObject
      "CreateBackendAuthOAuthConfig"
      ( \x ->
          CreateBackendAuthOAuthConfig'
            Prelude.<$> (x Core..:? "socialProviderSettings")
            Prelude.<*> (x Core..:? "domainPrefix")
            Prelude.<*> ( x Core..:? "redirectSignOutURIs"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "redirectSignInURIs"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "oAuthGrantType")
            Prelude.<*> (x Core..:? "oAuthScopes" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    CreateBackendAuthOAuthConfig
  where
  hashWithSalt salt' CreateBackendAuthOAuthConfig' {..} =
    salt' `Prelude.hashWithSalt` oAuthScopes
      `Prelude.hashWithSalt` oAuthGrantType
      `Prelude.hashWithSalt` redirectSignInURIs
      `Prelude.hashWithSalt` redirectSignOutURIs
      `Prelude.hashWithSalt` domainPrefix
      `Prelude.hashWithSalt` socialProviderSettings

instance Prelude.NFData CreateBackendAuthOAuthConfig where
  rnf CreateBackendAuthOAuthConfig' {..} =
    Prelude.rnf socialProviderSettings
      `Prelude.seq` Prelude.rnf oAuthScopes
      `Prelude.seq` Prelude.rnf oAuthGrantType
      `Prelude.seq` Prelude.rnf redirectSignInURIs
      `Prelude.seq` Prelude.rnf redirectSignOutURIs
      `Prelude.seq` Prelude.rnf domainPrefix

instance Core.ToJSON CreateBackendAuthOAuthConfig where
  toJSON CreateBackendAuthOAuthConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("socialProviderSettings" Core..=)
              Prelude.<$> socialProviderSettings,
            ("domainPrefix" Core..=) Prelude.<$> domainPrefix,
            Prelude.Just
              ("redirectSignOutURIs" Core..= redirectSignOutURIs),
            Prelude.Just
              ("redirectSignInURIs" Core..= redirectSignInURIs),
            Prelude.Just
              ("oAuthGrantType" Core..= oAuthGrantType),
            Prelude.Just ("oAuthScopes" Core..= oAuthScopes)
          ]
      )
