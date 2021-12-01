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
-- Module      : Amazonka.AmplifyBackend.Types.BackendAPIAppSyncAuthSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.BackendAPIAppSyncAuthSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The authentication settings for accessing provisioned data models in
-- your Amplify project.
--
-- /See:/ 'newBackendAPIAppSyncAuthSettings' smart constructor.
data BackendAPIAppSyncAuthSettings = BackendAPIAppSyncAuthSettings'
  { -- | The OpenID provider name, if OpenID was used as an authentication
    -- mechanism to access your data models.
    openIDProviderName :: Prelude.Maybe Prelude.Text,
    -- | The clientID for openID, if openID was used as an authentication setting
    -- to access your data models.
    openIDClientId :: Prelude.Maybe Prelude.Text,
    -- | The openID issuer URL, if openID was used as an authentication setting
    -- to access your data models.
    openIDIssueURL :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Cognito user pool ID, if Amazon Cognito was used as an
    -- authentication setting to access your data models.
    cognitoUserPoolId :: Prelude.Maybe Prelude.Text,
    -- | The expiry time for the OpenID authentication mechanism.
    openIDAuthTTL :: Prelude.Maybe Prelude.Text,
    -- | The API key description for API_KEY, if it was used as an authentication
    -- mechanism to access your data models.
    description :: Prelude.Maybe Prelude.Text,
    -- | The API key expiration time for API_KEY, if it was used as an
    -- authentication mechanism to access your data models.
    expirationTime :: Prelude.Maybe Prelude.Double,
    -- | The expiry time for the OpenID authentication mechanism.
    openIDIatTTL :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendAPIAppSyncAuthSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDProviderName', 'backendAPIAppSyncAuthSettings_openIDProviderName' - The OpenID provider name, if OpenID was used as an authentication
-- mechanism to access your data models.
--
-- 'openIDClientId', 'backendAPIAppSyncAuthSettings_openIDClientId' - The clientID for openID, if openID was used as an authentication setting
-- to access your data models.
--
-- 'openIDIssueURL', 'backendAPIAppSyncAuthSettings_openIDIssueURL' - The openID issuer URL, if openID was used as an authentication setting
-- to access your data models.
--
-- 'cognitoUserPoolId', 'backendAPIAppSyncAuthSettings_cognitoUserPoolId' - The Amazon Cognito user pool ID, if Amazon Cognito was used as an
-- authentication setting to access your data models.
--
-- 'openIDAuthTTL', 'backendAPIAppSyncAuthSettings_openIDAuthTTL' - The expiry time for the OpenID authentication mechanism.
--
-- 'description', 'backendAPIAppSyncAuthSettings_description' - The API key description for API_KEY, if it was used as an authentication
-- mechanism to access your data models.
--
-- 'expirationTime', 'backendAPIAppSyncAuthSettings_expirationTime' - The API key expiration time for API_KEY, if it was used as an
-- authentication mechanism to access your data models.
--
-- 'openIDIatTTL', 'backendAPIAppSyncAuthSettings_openIDIatTTL' - The expiry time for the OpenID authentication mechanism.
newBackendAPIAppSyncAuthSettings ::
  BackendAPIAppSyncAuthSettings
newBackendAPIAppSyncAuthSettings =
  BackendAPIAppSyncAuthSettings'
    { openIDProviderName =
        Prelude.Nothing,
      openIDClientId = Prelude.Nothing,
      openIDIssueURL = Prelude.Nothing,
      cognitoUserPoolId = Prelude.Nothing,
      openIDAuthTTL = Prelude.Nothing,
      description = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      openIDIatTTL = Prelude.Nothing
    }

-- | The OpenID provider name, if OpenID was used as an authentication
-- mechanism to access your data models.
backendAPIAppSyncAuthSettings_openIDProviderName :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_openIDProviderName = Lens.lens (\BackendAPIAppSyncAuthSettings' {openIDProviderName} -> openIDProviderName) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {openIDProviderName = a} :: BackendAPIAppSyncAuthSettings)

-- | The clientID for openID, if openID was used as an authentication setting
-- to access your data models.
backendAPIAppSyncAuthSettings_openIDClientId :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_openIDClientId = Lens.lens (\BackendAPIAppSyncAuthSettings' {openIDClientId} -> openIDClientId) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {openIDClientId = a} :: BackendAPIAppSyncAuthSettings)

-- | The openID issuer URL, if openID was used as an authentication setting
-- to access your data models.
backendAPIAppSyncAuthSettings_openIDIssueURL :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_openIDIssueURL = Lens.lens (\BackendAPIAppSyncAuthSettings' {openIDIssueURL} -> openIDIssueURL) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {openIDIssueURL = a} :: BackendAPIAppSyncAuthSettings)

-- | The Amazon Cognito user pool ID, if Amazon Cognito was used as an
-- authentication setting to access your data models.
backendAPIAppSyncAuthSettings_cognitoUserPoolId :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_cognitoUserPoolId = Lens.lens (\BackendAPIAppSyncAuthSettings' {cognitoUserPoolId} -> cognitoUserPoolId) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {cognitoUserPoolId = a} :: BackendAPIAppSyncAuthSettings)

-- | The expiry time for the OpenID authentication mechanism.
backendAPIAppSyncAuthSettings_openIDAuthTTL :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_openIDAuthTTL = Lens.lens (\BackendAPIAppSyncAuthSettings' {openIDAuthTTL} -> openIDAuthTTL) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {openIDAuthTTL = a} :: BackendAPIAppSyncAuthSettings)

-- | The API key description for API_KEY, if it was used as an authentication
-- mechanism to access your data models.
backendAPIAppSyncAuthSettings_description :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_description = Lens.lens (\BackendAPIAppSyncAuthSettings' {description} -> description) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {description = a} :: BackendAPIAppSyncAuthSettings)

-- | The API key expiration time for API_KEY, if it was used as an
-- authentication mechanism to access your data models.
backendAPIAppSyncAuthSettings_expirationTime :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Double)
backendAPIAppSyncAuthSettings_expirationTime = Lens.lens (\BackendAPIAppSyncAuthSettings' {expirationTime} -> expirationTime) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {expirationTime = a} :: BackendAPIAppSyncAuthSettings)

-- | The expiry time for the OpenID authentication mechanism.
backendAPIAppSyncAuthSettings_openIDIatTTL :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_openIDIatTTL = Lens.lens (\BackendAPIAppSyncAuthSettings' {openIDIatTTL} -> openIDIatTTL) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {openIDIatTTL = a} :: BackendAPIAppSyncAuthSettings)

instance Core.FromJSON BackendAPIAppSyncAuthSettings where
  parseJSON =
    Core.withObject
      "BackendAPIAppSyncAuthSettings"
      ( \x ->
          BackendAPIAppSyncAuthSettings'
            Prelude.<$> (x Core..:? "openIDProviderName")
            Prelude.<*> (x Core..:? "openIDClientId")
            Prelude.<*> (x Core..:? "openIDIssueURL")
            Prelude.<*> (x Core..:? "cognitoUserPoolId")
            Prelude.<*> (x Core..:? "openIDAuthTTL")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "expirationTime")
            Prelude.<*> (x Core..:? "openIDIatTTL")
      )

instance
  Prelude.Hashable
    BackendAPIAppSyncAuthSettings
  where
  hashWithSalt salt' BackendAPIAppSyncAuthSettings' {..} =
    salt' `Prelude.hashWithSalt` openIDIatTTL
      `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` openIDAuthTTL
      `Prelude.hashWithSalt` cognitoUserPoolId
      `Prelude.hashWithSalt` openIDIssueURL
      `Prelude.hashWithSalt` openIDClientId
      `Prelude.hashWithSalt` openIDProviderName

instance Prelude.NFData BackendAPIAppSyncAuthSettings where
  rnf BackendAPIAppSyncAuthSettings' {..} =
    Prelude.rnf openIDProviderName
      `Prelude.seq` Prelude.rnf openIDIatTTL
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf openIDAuthTTL
      `Prelude.seq` Prelude.rnf cognitoUserPoolId
      `Prelude.seq` Prelude.rnf openIDIssueURL
      `Prelude.seq` Prelude.rnf openIDClientId

instance Core.ToJSON BackendAPIAppSyncAuthSettings where
  toJSON BackendAPIAppSyncAuthSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("openIDProviderName" Core..=)
              Prelude.<$> openIDProviderName,
            ("openIDClientId" Core..=)
              Prelude.<$> openIDClientId,
            ("openIDIssueURL" Core..=)
              Prelude.<$> openIDIssueURL,
            ("cognitoUserPoolId" Core..=)
              Prelude.<$> cognitoUserPoolId,
            ("openIDAuthTTL" Core..=) Prelude.<$> openIDAuthTTL,
            ("description" Core..=) Prelude.<$> description,
            ("expirationTime" Core..=)
              Prelude.<$> expirationTime,
            ("openIDIatTTL" Core..=) Prelude.<$> openIDIatTTL
          ]
      )
