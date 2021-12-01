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
-- Module      : Amazonka.MediaLive.Types.NielsenNaesIiNw
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.NielsenNaesIiNw where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Nielsen Naes Ii Nw
--
-- /See:/ 'newNielsenNaesIiNw' smart constructor.
data NielsenNaesIiNw = NielsenNaesIiNw'
  { -- | Enter the check digit string for the watermark
    checkDigitString :: Prelude.Text,
    -- | Enter the Nielsen Source ID (SID) to include in the watermark
    sid :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NielsenNaesIiNw' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkDigitString', 'nielsenNaesIiNw_checkDigitString' - Enter the check digit string for the watermark
--
-- 'sid', 'nielsenNaesIiNw_sid' - Enter the Nielsen Source ID (SID) to include in the watermark
newNielsenNaesIiNw ::
  -- | 'checkDigitString'
  Prelude.Text ->
  -- | 'sid'
  Prelude.Double ->
  NielsenNaesIiNw
newNielsenNaesIiNw pCheckDigitString_ pSid_ =
  NielsenNaesIiNw'
    { checkDigitString =
        pCheckDigitString_,
      sid = pSid_
    }

-- | Enter the check digit string for the watermark
nielsenNaesIiNw_checkDigitString :: Lens.Lens' NielsenNaesIiNw Prelude.Text
nielsenNaesIiNw_checkDigitString = Lens.lens (\NielsenNaesIiNw' {checkDigitString} -> checkDigitString) (\s@NielsenNaesIiNw' {} a -> s {checkDigitString = a} :: NielsenNaesIiNw)

-- | Enter the Nielsen Source ID (SID) to include in the watermark
nielsenNaesIiNw_sid :: Lens.Lens' NielsenNaesIiNw Prelude.Double
nielsenNaesIiNw_sid = Lens.lens (\NielsenNaesIiNw' {sid} -> sid) (\s@NielsenNaesIiNw' {} a -> s {sid = a} :: NielsenNaesIiNw)

instance Core.FromJSON NielsenNaesIiNw where
  parseJSON =
    Core.withObject
      "NielsenNaesIiNw"
      ( \x ->
          NielsenNaesIiNw'
            Prelude.<$> (x Core..: "checkDigitString")
            Prelude.<*> (x Core..: "sid")
      )

instance Prelude.Hashable NielsenNaesIiNw where
  hashWithSalt salt' NielsenNaesIiNw' {..} =
    salt' `Prelude.hashWithSalt` sid
      `Prelude.hashWithSalt` checkDigitString

instance Prelude.NFData NielsenNaesIiNw where
  rnf NielsenNaesIiNw' {..} =
    Prelude.rnf checkDigitString
      `Prelude.seq` Prelude.rnf sid

instance Core.ToJSON NielsenNaesIiNw where
  toJSON NielsenNaesIiNw' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("checkDigitString" Core..= checkDigitString),
            Prelude.Just ("sid" Core..= sid)
          ]
      )
