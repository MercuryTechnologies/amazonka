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
-- Module      : Amazonka.CognitoIdentityProvider.Types.TokenValidityUnitsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.TokenValidityUnitsType where

import Amazonka.CognitoIdentityProvider.Types.TimeUnitsType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The data type for TokenValidityUnits that specifics the time
-- measurements for token validity.
--
-- /See:/ 'newTokenValidityUnitsType' smart constructor.
data TokenValidityUnitsType = TokenValidityUnitsType'
  { -- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in
    -- AccessTokenValidity, defaults to hours.
    accessToken :: Prelude.Maybe TimeUnitsType,
    -- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in
    -- RefreshTokenValidity, defaults to days.
    refreshToken :: Prelude.Maybe TimeUnitsType,
    -- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in
    -- IdTokenValidity, defaults to hours.
    idToken :: Prelude.Maybe TimeUnitsType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TokenValidityUnitsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'tokenValidityUnitsType_accessToken' - A time unit in “seconds”, “minutes”, “hours” or “days” for the value in
-- AccessTokenValidity, defaults to hours.
--
-- 'refreshToken', 'tokenValidityUnitsType_refreshToken' - A time unit in “seconds”, “minutes”, “hours” or “days” for the value in
-- RefreshTokenValidity, defaults to days.
--
-- 'idToken', 'tokenValidityUnitsType_idToken' - A time unit in “seconds”, “minutes”, “hours” or “days” for the value in
-- IdTokenValidity, defaults to hours.
newTokenValidityUnitsType ::
  TokenValidityUnitsType
newTokenValidityUnitsType =
  TokenValidityUnitsType'
    { accessToken =
        Prelude.Nothing,
      refreshToken = Prelude.Nothing,
      idToken = Prelude.Nothing
    }

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in
-- AccessTokenValidity, defaults to hours.
tokenValidityUnitsType_accessToken :: Lens.Lens' TokenValidityUnitsType (Prelude.Maybe TimeUnitsType)
tokenValidityUnitsType_accessToken = Lens.lens (\TokenValidityUnitsType' {accessToken} -> accessToken) (\s@TokenValidityUnitsType' {} a -> s {accessToken = a} :: TokenValidityUnitsType)

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in
-- RefreshTokenValidity, defaults to days.
tokenValidityUnitsType_refreshToken :: Lens.Lens' TokenValidityUnitsType (Prelude.Maybe TimeUnitsType)
tokenValidityUnitsType_refreshToken = Lens.lens (\TokenValidityUnitsType' {refreshToken} -> refreshToken) (\s@TokenValidityUnitsType' {} a -> s {refreshToken = a} :: TokenValidityUnitsType)

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in
-- IdTokenValidity, defaults to hours.
tokenValidityUnitsType_idToken :: Lens.Lens' TokenValidityUnitsType (Prelude.Maybe TimeUnitsType)
tokenValidityUnitsType_idToken = Lens.lens (\TokenValidityUnitsType' {idToken} -> idToken) (\s@TokenValidityUnitsType' {} a -> s {idToken = a} :: TokenValidityUnitsType)

instance Core.FromJSON TokenValidityUnitsType where
  parseJSON =
    Core.withObject
      "TokenValidityUnitsType"
      ( \x ->
          TokenValidityUnitsType'
            Prelude.<$> (x Core..:? "AccessToken")
            Prelude.<*> (x Core..:? "RefreshToken")
            Prelude.<*> (x Core..:? "IdToken")
      )

instance Prelude.Hashable TokenValidityUnitsType where
  hashWithSalt salt' TokenValidityUnitsType' {..} =
    salt' `Prelude.hashWithSalt` idToken
      `Prelude.hashWithSalt` refreshToken
      `Prelude.hashWithSalt` accessToken

instance Prelude.NFData TokenValidityUnitsType where
  rnf TokenValidityUnitsType' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf idToken
      `Prelude.seq` Prelude.rnf refreshToken

instance Core.ToJSON TokenValidityUnitsType where
  toJSON TokenValidityUnitsType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccessToken" Core..=) Prelude.<$> accessToken,
            ("RefreshToken" Core..=) Prelude.<$> refreshToken,
            ("IdToken" Core..=) Prelude.<$> idToken
          ]
      )
