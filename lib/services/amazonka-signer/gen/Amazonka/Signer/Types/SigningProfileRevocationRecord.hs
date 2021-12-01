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
-- Module      : Amazonka.Signer.Types.SigningProfileRevocationRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningProfileRevocationRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Revocation information for a signing profile.
--
-- /See:/ 'newSigningProfileRevocationRecord' smart constructor.
data SigningProfileRevocationRecord = SigningProfileRevocationRecord'
  { -- | The identity of the revoker.
    revokedBy :: Prelude.Maybe Prelude.Text,
    -- | The time when revocation becomes effective.
    revocationEffectiveFrom :: Prelude.Maybe Core.POSIX,
    -- | The time when the signing profile was revoked.
    revokedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningProfileRevocationRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revokedBy', 'signingProfileRevocationRecord_revokedBy' - The identity of the revoker.
--
-- 'revocationEffectiveFrom', 'signingProfileRevocationRecord_revocationEffectiveFrom' - The time when revocation becomes effective.
--
-- 'revokedAt', 'signingProfileRevocationRecord_revokedAt' - The time when the signing profile was revoked.
newSigningProfileRevocationRecord ::
  SigningProfileRevocationRecord
newSigningProfileRevocationRecord =
  SigningProfileRevocationRecord'
    { revokedBy =
        Prelude.Nothing,
      revocationEffectiveFrom = Prelude.Nothing,
      revokedAt = Prelude.Nothing
    }

-- | The identity of the revoker.
signingProfileRevocationRecord_revokedBy :: Lens.Lens' SigningProfileRevocationRecord (Prelude.Maybe Prelude.Text)
signingProfileRevocationRecord_revokedBy = Lens.lens (\SigningProfileRevocationRecord' {revokedBy} -> revokedBy) (\s@SigningProfileRevocationRecord' {} a -> s {revokedBy = a} :: SigningProfileRevocationRecord)

-- | The time when revocation becomes effective.
signingProfileRevocationRecord_revocationEffectiveFrom :: Lens.Lens' SigningProfileRevocationRecord (Prelude.Maybe Prelude.UTCTime)
signingProfileRevocationRecord_revocationEffectiveFrom = Lens.lens (\SigningProfileRevocationRecord' {revocationEffectiveFrom} -> revocationEffectiveFrom) (\s@SigningProfileRevocationRecord' {} a -> s {revocationEffectiveFrom = a} :: SigningProfileRevocationRecord) Prelude.. Lens.mapping Core._Time

-- | The time when the signing profile was revoked.
signingProfileRevocationRecord_revokedAt :: Lens.Lens' SigningProfileRevocationRecord (Prelude.Maybe Prelude.UTCTime)
signingProfileRevocationRecord_revokedAt = Lens.lens (\SigningProfileRevocationRecord' {revokedAt} -> revokedAt) (\s@SigningProfileRevocationRecord' {} a -> s {revokedAt = a} :: SigningProfileRevocationRecord) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON SigningProfileRevocationRecord where
  parseJSON =
    Core.withObject
      "SigningProfileRevocationRecord"
      ( \x ->
          SigningProfileRevocationRecord'
            Prelude.<$> (x Core..:? "revokedBy")
            Prelude.<*> (x Core..:? "revocationEffectiveFrom")
            Prelude.<*> (x Core..:? "revokedAt")
      )

instance
  Prelude.Hashable
    SigningProfileRevocationRecord
  where
  hashWithSalt
    salt'
    SigningProfileRevocationRecord' {..} =
      salt' `Prelude.hashWithSalt` revokedAt
        `Prelude.hashWithSalt` revocationEffectiveFrom
        `Prelude.hashWithSalt` revokedBy

instance
  Prelude.NFData
    SigningProfileRevocationRecord
  where
  rnf SigningProfileRevocationRecord' {..} =
    Prelude.rnf revokedBy
      `Prelude.seq` Prelude.rnf revokedAt
      `Prelude.seq` Prelude.rnf revocationEffectiveFrom
