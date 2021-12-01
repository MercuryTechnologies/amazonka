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
-- Module      : Amazonka.AppSync.Types.OpenIDConnectConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.OpenIDConnectConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an OpenID Connect configuration.
--
-- /See:/ 'newOpenIDConnectConfig' smart constructor.
data OpenIDConnectConfig = OpenIDConnectConfig'
  { -- | The number of milliseconds a token is valid after being authenticated.
    authTTL :: Prelude.Maybe Prelude.Integer,
    -- | The client identifier of the Relying party at the OpenID identity
    -- provider. This identifier is typically obtained when the Relying party
    -- is registered with the OpenID identity provider. You can specify a
    -- regular expression so the AppSync can validate against multiple client
    -- identifiers at a time.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The number of milliseconds a token is valid after being issued to a
    -- user.
    iatTTL :: Prelude.Maybe Prelude.Integer,
    -- | The issuer for the OpenID Connect configuration. The issuer returned by
    -- discovery must exactly match the value of @iss@ in the ID token.
    issuer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenIDConnectConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authTTL', 'openIDConnectConfig_authTTL' - The number of milliseconds a token is valid after being authenticated.
--
-- 'clientId', 'openIDConnectConfig_clientId' - The client identifier of the Relying party at the OpenID identity
-- provider. This identifier is typically obtained when the Relying party
-- is registered with the OpenID identity provider. You can specify a
-- regular expression so the AppSync can validate against multiple client
-- identifiers at a time.
--
-- 'iatTTL', 'openIDConnectConfig_iatTTL' - The number of milliseconds a token is valid after being issued to a
-- user.
--
-- 'issuer', 'openIDConnectConfig_issuer' - The issuer for the OpenID Connect configuration. The issuer returned by
-- discovery must exactly match the value of @iss@ in the ID token.
newOpenIDConnectConfig ::
  -- | 'issuer'
  Prelude.Text ->
  OpenIDConnectConfig
newOpenIDConnectConfig pIssuer_ =
  OpenIDConnectConfig'
    { authTTL = Prelude.Nothing,
      clientId = Prelude.Nothing,
      iatTTL = Prelude.Nothing,
      issuer = pIssuer_
    }

-- | The number of milliseconds a token is valid after being authenticated.
openIDConnectConfig_authTTL :: Lens.Lens' OpenIDConnectConfig (Prelude.Maybe Prelude.Integer)
openIDConnectConfig_authTTL = Lens.lens (\OpenIDConnectConfig' {authTTL} -> authTTL) (\s@OpenIDConnectConfig' {} a -> s {authTTL = a} :: OpenIDConnectConfig)

-- | The client identifier of the Relying party at the OpenID identity
-- provider. This identifier is typically obtained when the Relying party
-- is registered with the OpenID identity provider. You can specify a
-- regular expression so the AppSync can validate against multiple client
-- identifiers at a time.
openIDConnectConfig_clientId :: Lens.Lens' OpenIDConnectConfig (Prelude.Maybe Prelude.Text)
openIDConnectConfig_clientId = Lens.lens (\OpenIDConnectConfig' {clientId} -> clientId) (\s@OpenIDConnectConfig' {} a -> s {clientId = a} :: OpenIDConnectConfig)

-- | The number of milliseconds a token is valid after being issued to a
-- user.
openIDConnectConfig_iatTTL :: Lens.Lens' OpenIDConnectConfig (Prelude.Maybe Prelude.Integer)
openIDConnectConfig_iatTTL = Lens.lens (\OpenIDConnectConfig' {iatTTL} -> iatTTL) (\s@OpenIDConnectConfig' {} a -> s {iatTTL = a} :: OpenIDConnectConfig)

-- | The issuer for the OpenID Connect configuration. The issuer returned by
-- discovery must exactly match the value of @iss@ in the ID token.
openIDConnectConfig_issuer :: Lens.Lens' OpenIDConnectConfig Prelude.Text
openIDConnectConfig_issuer = Lens.lens (\OpenIDConnectConfig' {issuer} -> issuer) (\s@OpenIDConnectConfig' {} a -> s {issuer = a} :: OpenIDConnectConfig)

instance Core.FromJSON OpenIDConnectConfig where
  parseJSON =
    Core.withObject
      "OpenIDConnectConfig"
      ( \x ->
          OpenIDConnectConfig'
            Prelude.<$> (x Core..:? "authTTL")
            Prelude.<*> (x Core..:? "clientId")
            Prelude.<*> (x Core..:? "iatTTL")
            Prelude.<*> (x Core..: "issuer")
      )

instance Prelude.Hashable OpenIDConnectConfig where
  hashWithSalt salt' OpenIDConnectConfig' {..} =
    salt' `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` iatTTL
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` authTTL

instance Prelude.NFData OpenIDConnectConfig where
  rnf OpenIDConnectConfig' {..} =
    Prelude.rnf authTTL
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf iatTTL
      `Prelude.seq` Prelude.rnf clientId

instance Core.ToJSON OpenIDConnectConfig where
  toJSON OpenIDConnectConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("authTTL" Core..=) Prelude.<$> authTTL,
            ("clientId" Core..=) Prelude.<$> clientId,
            ("iatTTL" Core..=) Prelude.<$> iatTTL,
            Prelude.Just ("issuer" Core..= issuer)
          ]
      )
