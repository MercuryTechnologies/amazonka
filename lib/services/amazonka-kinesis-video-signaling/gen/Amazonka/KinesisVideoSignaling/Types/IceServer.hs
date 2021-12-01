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
-- Module      : Amazonka.KinesisVideoSignaling.Types.IceServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoSignaling.Types.IceServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure for the ICE server connection data.
--
-- /See:/ 'newIceServer' smart constructor.
data IceServer = IceServer'
  { -- | The period of time, in seconds, during which the username and password
    -- are valid.
    ttl :: Prelude.Maybe Prelude.Natural,
    -- | An array of URIs, in the form specified in the
    -- <https://tools.ietf.org/html/draft-petithuguenin-behave-turn-uris-03 I-D.petithuguenin-behave-turn-uris>
    -- spec. These URIs provide the different addresses and\/or protocols that
    -- can be used to reach the TURN server.
    uris :: Prelude.Maybe [Prelude.Text],
    -- | A username to login to the ICE server.
    username :: Prelude.Maybe Prelude.Text,
    -- | A password to login to the ICE server.
    password :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ttl', 'iceServer_ttl' - The period of time, in seconds, during which the username and password
-- are valid.
--
-- 'uris', 'iceServer_uris' - An array of URIs, in the form specified in the
-- <https://tools.ietf.org/html/draft-petithuguenin-behave-turn-uris-03 I-D.petithuguenin-behave-turn-uris>
-- spec. These URIs provide the different addresses and\/or protocols that
-- can be used to reach the TURN server.
--
-- 'username', 'iceServer_username' - A username to login to the ICE server.
--
-- 'password', 'iceServer_password' - A password to login to the ICE server.
newIceServer ::
  IceServer
newIceServer =
  IceServer'
    { ttl = Prelude.Nothing,
      uris = Prelude.Nothing,
      username = Prelude.Nothing,
      password = Prelude.Nothing
    }

-- | The period of time, in seconds, during which the username and password
-- are valid.
iceServer_ttl :: Lens.Lens' IceServer (Prelude.Maybe Prelude.Natural)
iceServer_ttl = Lens.lens (\IceServer' {ttl} -> ttl) (\s@IceServer' {} a -> s {ttl = a} :: IceServer)

-- | An array of URIs, in the form specified in the
-- <https://tools.ietf.org/html/draft-petithuguenin-behave-turn-uris-03 I-D.petithuguenin-behave-turn-uris>
-- spec. These URIs provide the different addresses and\/or protocols that
-- can be used to reach the TURN server.
iceServer_uris :: Lens.Lens' IceServer (Prelude.Maybe [Prelude.Text])
iceServer_uris = Lens.lens (\IceServer' {uris} -> uris) (\s@IceServer' {} a -> s {uris = a} :: IceServer) Prelude.. Lens.mapping Lens.coerced

-- | A username to login to the ICE server.
iceServer_username :: Lens.Lens' IceServer (Prelude.Maybe Prelude.Text)
iceServer_username = Lens.lens (\IceServer' {username} -> username) (\s@IceServer' {} a -> s {username = a} :: IceServer)

-- | A password to login to the ICE server.
iceServer_password :: Lens.Lens' IceServer (Prelude.Maybe Prelude.Text)
iceServer_password = Lens.lens (\IceServer' {password} -> password) (\s@IceServer' {} a -> s {password = a} :: IceServer)

instance Core.FromJSON IceServer where
  parseJSON =
    Core.withObject
      "IceServer"
      ( \x ->
          IceServer'
            Prelude.<$> (x Core..:? "Ttl")
            Prelude.<*> (x Core..:? "Uris" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "Password")
      )

instance Prelude.Hashable IceServer where
  hashWithSalt salt' IceServer' {..} =
    salt' `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` uris
      `Prelude.hashWithSalt` ttl

instance Prelude.NFData IceServer where
  rnf IceServer' {..} =
    Prelude.rnf ttl `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf uris
