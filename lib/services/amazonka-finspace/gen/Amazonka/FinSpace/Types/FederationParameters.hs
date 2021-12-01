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
-- Module      : Amazonka.FinSpace.Types.FederationParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.FederationParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration information when authentication mode is FEDERATED.
--
-- /See:/ 'newFederationParameters' smart constructor.
data FederationParameters = FederationParameters'
  { -- | Provide the metadata URL from your SAML 2.0 compliant identity provider
    -- (IdP).
    samlMetadataURL :: Prelude.Maybe Prelude.Text,
    -- | The redirect or sign-in URL that should be entered into the SAML 2.0
    -- compliant identity provider configuration (IdP).
    applicationCallBackURL :: Prelude.Maybe Prelude.Text,
    -- | The Uniform Resource Name (URN). Also referred as Service Provider URN
    -- or Audience URI or Service Provider Entity ID.
    federationURN :: Prelude.Maybe Prelude.Text,
    -- | SAML attribute name and value. The name must always be @Email@ and the
    -- value should be set to the attribute definition in which user email is
    -- set. For example, name would be @Email@ and value
    -- @http:\/\/schemas.xmlsoap.org\/ws\/2005\/05\/identity\/claims\/emailaddress@.
    -- Please check your SAML 2.0 compliant identity provider (IdP)
    -- documentation for details.
    attributeMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Name of the identity provider (IdP).
    federationProviderName :: Prelude.Maybe Prelude.Text,
    -- | SAML 2.0 Metadata document from identity provider (IdP).
    samlMetadataDocument :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FederationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samlMetadataURL', 'federationParameters_samlMetadataURL' - Provide the metadata URL from your SAML 2.0 compliant identity provider
-- (IdP).
--
-- 'applicationCallBackURL', 'federationParameters_applicationCallBackURL' - The redirect or sign-in URL that should be entered into the SAML 2.0
-- compliant identity provider configuration (IdP).
--
-- 'federationURN', 'federationParameters_federationURN' - The Uniform Resource Name (URN). Also referred as Service Provider URN
-- or Audience URI or Service Provider Entity ID.
--
-- 'attributeMap', 'federationParameters_attributeMap' - SAML attribute name and value. The name must always be @Email@ and the
-- value should be set to the attribute definition in which user email is
-- set. For example, name would be @Email@ and value
-- @http:\/\/schemas.xmlsoap.org\/ws\/2005\/05\/identity\/claims\/emailaddress@.
-- Please check your SAML 2.0 compliant identity provider (IdP)
-- documentation for details.
--
-- 'federationProviderName', 'federationParameters_federationProviderName' - Name of the identity provider (IdP).
--
-- 'samlMetadataDocument', 'federationParameters_samlMetadataDocument' - SAML 2.0 Metadata document from identity provider (IdP).
newFederationParameters ::
  FederationParameters
newFederationParameters =
  FederationParameters'
    { samlMetadataURL =
        Prelude.Nothing,
      applicationCallBackURL = Prelude.Nothing,
      federationURN = Prelude.Nothing,
      attributeMap = Prelude.Nothing,
      federationProviderName = Prelude.Nothing,
      samlMetadataDocument = Prelude.Nothing
    }

-- | Provide the metadata URL from your SAML 2.0 compliant identity provider
-- (IdP).
federationParameters_samlMetadataURL :: Lens.Lens' FederationParameters (Prelude.Maybe Prelude.Text)
federationParameters_samlMetadataURL = Lens.lens (\FederationParameters' {samlMetadataURL} -> samlMetadataURL) (\s@FederationParameters' {} a -> s {samlMetadataURL = a} :: FederationParameters)

-- | The redirect or sign-in URL that should be entered into the SAML 2.0
-- compliant identity provider configuration (IdP).
federationParameters_applicationCallBackURL :: Lens.Lens' FederationParameters (Prelude.Maybe Prelude.Text)
federationParameters_applicationCallBackURL = Lens.lens (\FederationParameters' {applicationCallBackURL} -> applicationCallBackURL) (\s@FederationParameters' {} a -> s {applicationCallBackURL = a} :: FederationParameters)

-- | The Uniform Resource Name (URN). Also referred as Service Provider URN
-- or Audience URI or Service Provider Entity ID.
federationParameters_federationURN :: Lens.Lens' FederationParameters (Prelude.Maybe Prelude.Text)
federationParameters_federationURN = Lens.lens (\FederationParameters' {federationURN} -> federationURN) (\s@FederationParameters' {} a -> s {federationURN = a} :: FederationParameters)

-- | SAML attribute name and value. The name must always be @Email@ and the
-- value should be set to the attribute definition in which user email is
-- set. For example, name would be @Email@ and value
-- @http:\/\/schemas.xmlsoap.org\/ws\/2005\/05\/identity\/claims\/emailaddress@.
-- Please check your SAML 2.0 compliant identity provider (IdP)
-- documentation for details.
federationParameters_attributeMap :: Lens.Lens' FederationParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
federationParameters_attributeMap = Lens.lens (\FederationParameters' {attributeMap} -> attributeMap) (\s@FederationParameters' {} a -> s {attributeMap = a} :: FederationParameters) Prelude.. Lens.mapping Lens.coerced

-- | Name of the identity provider (IdP).
federationParameters_federationProviderName :: Lens.Lens' FederationParameters (Prelude.Maybe Prelude.Text)
federationParameters_federationProviderName = Lens.lens (\FederationParameters' {federationProviderName} -> federationProviderName) (\s@FederationParameters' {} a -> s {federationProviderName = a} :: FederationParameters)

-- | SAML 2.0 Metadata document from identity provider (IdP).
federationParameters_samlMetadataDocument :: Lens.Lens' FederationParameters (Prelude.Maybe Prelude.Text)
federationParameters_samlMetadataDocument = Lens.lens (\FederationParameters' {samlMetadataDocument} -> samlMetadataDocument) (\s@FederationParameters' {} a -> s {samlMetadataDocument = a} :: FederationParameters)

instance Core.FromJSON FederationParameters where
  parseJSON =
    Core.withObject
      "FederationParameters"
      ( \x ->
          FederationParameters'
            Prelude.<$> (x Core..:? "samlMetadataURL")
            Prelude.<*> (x Core..:? "applicationCallBackURL")
            Prelude.<*> (x Core..:? "federationURN")
            Prelude.<*> (x Core..:? "attributeMap" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "federationProviderName")
            Prelude.<*> (x Core..:? "samlMetadataDocument")
      )

instance Prelude.Hashable FederationParameters where
  hashWithSalt salt' FederationParameters' {..} =
    salt' `Prelude.hashWithSalt` samlMetadataDocument
      `Prelude.hashWithSalt` federationProviderName
      `Prelude.hashWithSalt` attributeMap
      `Prelude.hashWithSalt` federationURN
      `Prelude.hashWithSalt` applicationCallBackURL
      `Prelude.hashWithSalt` samlMetadataURL

instance Prelude.NFData FederationParameters where
  rnf FederationParameters' {..} =
    Prelude.rnf samlMetadataURL
      `Prelude.seq` Prelude.rnf samlMetadataDocument
      `Prelude.seq` Prelude.rnf federationProviderName
      `Prelude.seq` Prelude.rnf attributeMap
      `Prelude.seq` Prelude.rnf federationURN
      `Prelude.seq` Prelude.rnf applicationCallBackURL

instance Core.ToJSON FederationParameters where
  toJSON FederationParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("samlMetadataURL" Core..=)
              Prelude.<$> samlMetadataURL,
            ("applicationCallBackURL" Core..=)
              Prelude.<$> applicationCallBackURL,
            ("federationURN" Core..=) Prelude.<$> federationURN,
            ("attributeMap" Core..=) Prelude.<$> attributeMap,
            ("federationProviderName" Core..=)
              Prelude.<$> federationProviderName,
            ("samlMetadataDocument" Core..=)
              Prelude.<$> samlMetadataDocument
          ]
      )
